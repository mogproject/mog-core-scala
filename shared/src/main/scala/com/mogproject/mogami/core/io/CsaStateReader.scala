package com.mogproject.mogami.core.io

import com.mogproject.mogami._
import com.mogproject.mogami.util.MapUtil
import com.mogproject.mogami.util.Implicits._

import scala.annotation.tailrec
import scala.util.Try

/**
  * Reads CSA-formatted state
  */
trait CsaStateReader extends CsaFactory[State] {
  type PtypeMap = Map[Ptype, Int]
  type Result = (BoardType, HandType, PtypeMap)

  /**
    * parse init expression
    *
    * @param s string expression
    * @return parsed result (board and hand) or None (if failed)
    */
  protected[io] def parseInitExpression(s: String): Option[Result] = {
    @tailrec
    def f(sofar: Option[Result], ls: List[String]): Option[Result] = (sofar, ls) match {
      case (Some((b, h, rest)), x :: xs) if x.length == 4 =>
        val next = for {
          pos <- Square.parseCsaString(x.substring(0, 2))
          pt <- Ptype.parseCsaString(x.substring(2, 4))
          piece <- b.get(pos)
          if pt == piece.ptype
        } yield {
          (b - pos, h, MapUtil.incrementMap(rest, pt))
        }
        f(next, xs)
      case (_, Nil) => sofar
      case _ => None
    }

    if (s.startsWith("PI")) {
      f(Some(State.HIRATE.board, State.HIRATE.hand, State.capacity.mapValues(_ => 0)), s.substring(2).grouped(4).toList)
    } else {
      None
    }
  }

  /**
    * parse bundle expression
    *
    * @param lines string expression
    * @return parsed result (board and hand) or None (if failed)
    * @see http://www2.computer-shogi.org/protocol/record_v22.html
    *      "先後の区別が"+""-"以外のとき、駒がないとする。"
    */
  protected[io] def parseBundleExpression(result: Option[Result], lines: List[String]): Option[Result] = {
    @tailrec
    def f(sofar: Option[Result], ls: List[String], rank: Int): Option[Result] = (sofar, ls, rank) match {
      case (Some(_), x :: xs, _) if x.length == 2 + 3 * 9 && x.startsWith(s"P${rank}") =>
        f(parseOneLine(sofar, x.drop(2).grouped(3).toList, 9, rank), xs, rank + 1)
      case (_, Nil, 10) => sofar
      case _ => None
    }

    @tailrec
    def parseOneLine(sofar: Option[Result], ls: List[String], file: Int, rank: Int): Option[Result] = (sofar, ls, file) match {
      case (Some(_), x :: xs, _) if !List("+", "-").contains(x.take(1)) => // no piece
        parseOneLine(sofar, xs, file - 1, rank)
      case (Some((b, h, rest)), x :: xs, _) => // add one piece
        val pos = Square(file, rank)
        val p = Piece.parseCsaString(x)
        val pt = p.map(_.ptype.demoted)

        // todo: use match syntax
        if (p.isDefined && rest.getOrElse(pt.get, 0) >= 1) {
          parseOneLine(Some((b.updated(pos, p.get), h, MapUtil.decrementMap(rest, pt.get))), xs, file - 1, rank)
        } else {
          None // invalid piece string
        }
      case (_, Nil, 0) => sofar
      case _ => None
    }

    f(result, lines, 1)
  }

  protected[io] def parseSingleExpression(result: Option[Result], s: String): Option[(Result, Boolean)] = {
    @tailrec
    def f(sofar: Option[(Result, Boolean)], ls: List[String], t: Player) : Option[(Result, Boolean)] = (sofar, ls) match {
      case (Some(((b, h, rest), _)), "00AL" :: Nil) =>
        val newHand = MapUtil.mergeMaps(h, rest.collect { case (k, v) if k != KING => Hand(t, k) -> v })(_ + _, 0)
        f(Some(((b, newHand, State.capacity.mapValues(_ => 0)), true)), Nil, t)
      case (Some(((b, h, rest), used)), x :: xs) if x.length == 4 =>
        val isHand = x.startsWith("00")
        val next = for {
          pos <- if (isHand) Some(Square(0)) else Square.parseCsaString(x.substring(0, 2))
          pt <- Ptype.parseCsaString(x.substring(2, 4))
          if isHand || !b.contains(pos)  // the square must be unused
          if !isHand || pt.isHandType  // check the piece type
          if rest.getOrElse(pt.demoted, 0) >= 1  // check the number of piece types
        } yield {
          val p = Piece(t, pt)
          val newBoard = (!isHand).when[BoardType](_.updated(pos, p))(b)
          val newHand = isHand.when[HandType](MapUtil.incrementMap(_, Hand(t, pt)))(h)
          val newRest = MapUtil.decrementMap(rest, pt.demoted)
          ((newBoard, newHand, newRest), used)
        }
        f(next, xs, t)
      case (_, Nil) => sofar
      case _ => None
    }

    for {
      t <- Player.parseCsaString(s.slice(1, 2)) if s.startsWith("P")
      r <- result
      x <- f(Some(r, false), s.drop(2).grouped(4).toList, t)
    } yield x
  }

  def parseCsaString(s: String): Option[State] = {
    @tailrec
    def f(ss: List[String], sofar: Option[Result], usedInit: Boolean, usedAll: Boolean): Option[State] = {
      (ss, sofar, usedInit, usedAll) match {
        case (x :: Nil, Some((b, h, _)), _, _) => // Turn to move should be written in the last line
          Player.parseCsaString(x) flatMap { t => Try(State(t, b, h)).toOption }
        case (x :: xs, Some(_), false, false) if x.startsWith("PI") => // Initialize with Hirate and handicap
          f(xs, parseInitExpression(x), usedInit = true, usedAll = false)
        case (x :: xs, Some(_), false, false) if x.startsWith("P1") =>
          f(ss.drop(9), parseBundleExpression(sofar, ss.take(9)), usedInit = true, usedAll = false)
        case (x :: xs, Some(_), _, false) if x.startsWith("P+") || x.startsWith("P-") =>
          parseSingleExpression(sofar, x) match {
            case Some((t, u)) => f(xs, Some(t), usedInit, u)
            case _ => None
          }
        case _ =>
          None
      }
    }

    f(s.split('\n').toList, Some((Map.empty, State.EMPTY_HANDS, State.capacity)), usedInit = false, usedAll = false)
  }

}
