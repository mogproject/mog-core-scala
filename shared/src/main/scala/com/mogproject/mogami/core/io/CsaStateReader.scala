package com.mogproject.mogami.core.io

import com.mogproject.mogami._
import com.mogproject.mogami.util.MapUtil

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

/**
  * Reads CSA-formatted state
  */
trait CsaStateReader extends CsaFactory[State] {
  type PtypeMap = Map[Ptype, Int]
  type Result = (BoardType, HandType, PtypeMap)

  /**
    * parse init expression
    *
    * @param lineNo line number
    * @param s      line string
    * @return parsed result (board and hand)
    */
  protected[io] def parseInitExpression(lineNo: LineNo, s: String): Result = {
    @tailrec
    def f(sofar: Result, ls: List[String]): Result = (sofar, ls) match {
      case ((b, h, rest), x :: xs) if x.length == 4 =>
        val pos = Square.parseCsaString(NonEmptyLines(lineNo, x.substring(0, 2)))
        val pt = Ptype.parseCsaString(NonEmptyLines(lineNo, x.substring(2, 4)))
        val piece = b.getOrElse(pos, throw new RecordFormatException(lineNo, s"unmatched piece position: ${x}"))
        if (pt != piece.ptype) throw new RecordFormatException(lineNo, s"unmatched piece type: ${x}")
        f((b - pos, h, MapUtil.incrementMap(rest, pt)), xs)
      case (_, x :: _) => throw new RecordFormatException(lineNo, s"invalid init format: ${x}")
      case (_, Nil) => sofar

    }

    if (s.startsWith("PI")) {
      f((State.HIRATE.board, State.HIRATE.hand, State.capacity.mapValues(_ => 0)), s.substring(2).grouped(4).toList)
    } else {
      throw new RecordFormatException(lineNo, s"invalid init expression: ${s}")
    }
  }

  /**
    * parse bundle expression
    *
    * @param nel non-empty sequence of string expressions
    * @return parsed result (board and hand)
    * @see http://www2.computer-shogi.org/protocol/record_v22.html
    *      "先後の区別が"+""-"以外のとき、駒がないとする。"
    */
  protected[io] def parseBundleExpression(result: Result, nel: NonEmptyLines): Result = {
    @tailrec
    def f(sofar: Result, ls: List[Line], rank: Int): Result = (ls, rank) match {
      case ((x, n) :: xs, _) if x.length == 2 + 3 * 9 && x.startsWith(s"P${rank}") =>
        f(parseOneLine(sofar, n, x.drop(2).grouped(3).toList, 9, rank), xs, rank + 1)
      case ((x, n) :: _, _) =>
        throw new RecordFormatException(n, s"""invalid bundle line (rank=${rank}): "${x}""""")
      case (Nil, 10) => sofar
      case _ => throw new RecordFormatException(nel.lines.last._2, s"incomplete bundle expression (rank=${rank})")
    }

    @tailrec
    def parseOneLine(sofar: Result, lineNo: LineNo, ls: List[String], file: Int, rank: Int): Result = ls match {
      case x :: xs if !List("+", "-").contains(x.take(1)) => // no piece
        parseOneLine(sofar, lineNo, xs, file - 1, rank)
      case x :: xs => // add one piece
        val pos = Square(file, rank)
        val p = Piece.parseCsaString(NonEmptyLines(lineNo, x))
        val pt = p.ptype.demoted

        val (b, h, rest) = sofar
        if (rest.getOrElse(pt, 0) == 0) throw new RecordFormatException(lineNo, s"not enough piece left: ${x}")
        parseOneLine((b.updated(pos, p), h, MapUtil.decrementMap(rest, pt)), lineNo, xs, file - 1, rank)
      case Nil => sofar
    }

    f(result, nel.lines.toList, 1)
  }

  protected[io] def parseSingleExpression(result: Result, lineNo: LineNo, s: String): (Result, Boolean) = {
    @tailrec
    def f(sofar: (Result, Boolean), ls: List[String], t: Player): (Result, Boolean) = (sofar, ls) match {
      case (((b, h, rest), false), "00AL" :: Nil) =>
        val newHand = MapUtil.mergeMaps(h, rest.collect { case (k, v) if k != KING => Hand(t, k) -> v })(_ + _, 0)
        f(((b, newHand, State.capacity.mapValues(_ => 0)), true), Nil, t)
      case (((b, h, rest), false), x :: xs) if x.length == 4 =>
        val isHand = x.startsWith("00")
        val pt = Ptype.parseCsaString(NonEmptyLines(lineNo, x.substring(2, 4)))
        if (rest.getOrElse(pt.demoted, 0) == 0) throw new RecordFormatException(lineNo, s"not enough piece left: ${x}")
        val newRest = MapUtil.decrementMap(rest, pt.demoted)

        val (newBoard, newHand) = if (isHand) {
          if (!pt.isHandType) throw new RecordFormatException(lineNo, s"invalid piece type for hand: ${x}")
          (b, MapUtil.incrementMap(h, Hand(t, pt)))
        } else {
          val pos = Square.parseCsaString(NonEmptyLines(lineNo, x.substring(0, 2)))
          if (b.contains(pos)) throw new RecordFormatException(lineNo, s"position already used: ${x}")
          (b.updated(pos, Piece(t, pt)), h)
        }
        f(((newBoard, newHand, newRest), false), xs, t)
      case ((_, true), x :: _) => throw new RecordFormatException(lineNo, s"'AL' flag already used: ${x}")
      case ((_, false), x :: _) => throw new RecordFormatException(lineNo, s"invalid single format: ${x}")
      case (_, Nil) => sofar
    }

    if (s.startsWith("P")) {
      val t = Player.parseCsaString(NonEmptyLines(lineNo, s.slice(1, 2)))
      f((result, false), s.drop(2).grouped(4).toList, t)
    } else {
      throw new RecordFormatException(lineNo, s"invalid single expression: ${s}")
    }
  }

  def parseCsaString(nel: NonEmptyLines): State = {
    @tailrec
    def f(ls: List[Line], sofar: Result, usedInit: Boolean, usedAll: Boolean): State = {
      (ls, usedInit, usedAll) match {
        case (ln :: Nil, _, _) => // Turn to move should be written in the last line
          val t = Player.parseCsaString(NonEmptyLines(ln))
          Try(State(t, sofar._1, sofar._2)) match {
            case Success(st) => st
            case Failure(e) => throw new RecordFormatException(ln._2, s"invalid state: ${e}")
          }
        case ((x, n) :: xs, false, false) if x.startsWith("PI") => // Initialize with Hirate and handicap
          f(xs, parseInitExpression(n, x), usedInit = true, usedAll = false)
        case ((x, _) :: xs, false, false) if x.startsWith("P1") =>
          f(ls.drop(9), parseBundleExpression(sofar, NonEmptyLines(ls.take(9))), usedInit = true, usedAll = false)
        case ((x, n) :: xs, _, false) if x.startsWith("P+") || x.startsWith("P-") =>
          val (r, u) = parseSingleExpression(sofar, n, x)
          f(xs, r, usedInit, u)
        case ((x, n) :: _, _, _) => throw new RecordFormatException(n, s"invalid state format: ${x}")
        case (Nil, _, _) => throw new RecordFormatException(nel.lines.last._2, "incomplete state expression")
      }
    }

    f(nel.lines.toList, (Map.empty, State.EMPTY_HANDS, State.capacity), usedInit = false, usedAll = false)
  }

}
