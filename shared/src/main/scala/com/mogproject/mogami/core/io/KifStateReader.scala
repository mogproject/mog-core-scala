package com.mogproject.mogami.core.io

import com.mogproject.mogami._
import com.mogproject.mogami.core.state
import com.mogproject.mogami.util.Implicits._

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

/**
  * Reads Kif(Ki2)-formatted state
  */
trait KifStateReader extends KifFactory[state.State] {

  private[this] def parseNumber(lineNo: LineNo, s: String, default: Int): Int = {
    def f(c: Char): Option[Int] = "一二三四五六七八九".indexOf(c) match {
      case n if n >= 0 => Some(n + 1)
      case _ => None
    }

    (s.toList match {
      case Nil => Some(default)
      case '十' :: Nil => Some(10)
      case '十' :: c :: Nil => f(c).map(_ + 10)
      case c :: Nil => f(c)
      case _ => None
    }).getOrElse(throw new RecordFormatException(lineNo, s"number format error: ${s}"))
  }

  protected[io] def parseHandExpression(lineNo: LineNo, s: String, player: Player): HandType = {
    def f(h: String): (Hand, Int) = {
      val pt = Ptype.parseKifString(NonEmptyLines(lineNo, h.take(1)))
      val n = parseNumber(lineNo, h.drop(1), 1)
      (Hand(player, pt), n)
    }

    if (s == "なし") {
      Map.empty
    } else {
      s.split('　').map(f).foldLeft[HandType](Map.empty)(_ + _)
    }
  }

  protected[io] def parseBoardExpression(nel: NonEmptyLines): BoardType = {
    @tailrec
    def f(sofar: BoardType, ls: List[Line], rank: Int): BoardType = (ls, rank) match {
      case ((x, n) :: xs, _) if x.length == 3 + 2 * 9 =>
        f(parseOneLine(sofar, n, x.slice(1, 1 + 2 * 9).grouped(2).toList, 9, rank), xs, rank + 1)
      case (Nil, 10) => sofar
      case _ => throw new RecordFormatException(nel.lines.last._2, s"incomplete board expression (rank=${rank})")
    }

    def parseOneLine(sofar: BoardType, lineNo: LineNo, ls: List[String], file: Int, rank: Int): BoardType = ls match {
      case x :: xs if x == " ・" => // no piece
        parseOneLine(sofar, lineNo, xs, file - 1, rank)
      case x :: xs => // add one piece
        val pos = Square(file, rank)
        val p = Piece.parseKifString(NonEmptyLines(lineNo, x))

        parseOneLine(sofar.updated(pos, p), lineNo, xs, file - 1, rank)
      case Nil => sofar
    }

    f(Map.empty, nel.lines.slice(2, 11).toList, 1)
  }

  override def parseKifString(nel: NonEmptyLines): state.State = {
    @tailrec
    def f(ls: List[Line], sofar: (BoardType, HandType)): state.State = {
      ls match {
        case (x, n) :: xs if x.slice(1, 6) == "手の持駒：" =>
          f(xs, (
            sofar._1,
            sofar._2 ++ parseHandExpression(n, x.drop(6), x.headOption.exists(Seq('先', '下').contains).fold(BLACK, WHITE))
          ))
        case _ if ls.length <= 1 => // Turn to move should be written in the last line
          val s = ls.map(_._1).mkString
          val t = Map("" -> BLACK, "後手番" -> WHITE, "上手番" -> WHITE).getOrElse(
            s, throw new RecordFormatException(ls.head._2, s"unknown turn expression: ${ls.head._1}")
          )
          Try(State(t, sofar._1, sofar._2)) match {
            case Success(st) => st
            case Failure(e) => throw new RecordFormatException(nel.lines.last._2, s"invalid state: ${e}")
          }
        case (x, _) :: _ if x == "  ９ ８ ７ ６ ５ ４ ３ ２ １" =>
          val b = parseBoardExpression(NonEmptyLines(ls.take(12)))
          f(ls.drop(12), (b, sofar._2))
        case (x, n) :: _ => throw new RecordFormatException(n, s"invalid state format: ${x}")
        case Nil => throw new RecordFormatException(nel.lines.last._2, "incomplete state expression")
      }
    }

    f(nel.lines.toList, (Map.empty, State.EMPTY_HANDS))
  }

}
