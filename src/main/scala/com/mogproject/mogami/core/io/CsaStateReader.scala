package com.mogproject.mogami.core.io

import com.mogproject.mogami.core._
import com.mogproject.mogami.util.MapUtil

import scala.annotation.tailrec

/**
  * Reads CSA-formatted state
  */
trait CsaStateReader extends CsaFactory[State] {

  protected[io] def parseInitExpression(s: String): Option[State] = {
    @tailrec
    def f(sofar: Option[State], ls: List[String]): Option[State] = (sofar, ls) match {
      case (Some(st), x :: xs) if x.length == 4 =>
        val next = for {
          pos <- Square.parseCsaString(x.substring(0, 2))
          pt <- Ptype.parseCsaString(x.substring(2, 4))
          piece <- st.board.get(pos) if pt == piece.ptype
        } yield {
          State(st.turn, st.board - pos, st.hand)
        }
        f(next, xs)
      case (_, Nil) => sofar
      case _ => None
    }

    if (s.startsWith("PI")) {
      f(Some(State.HIRATE), s.substring(2).grouped(4).toList)
    } else {
      None
    }
  }

  /**
    * parse bundle expression
    * @param lines string expression
    * @return parsed state or None (if failed)
    *
    * @see http://www2.computer-shogi.org/protocol/record_v22.html
    *      "先後の区別が"+""-"以外のとき、駒がないとする。"
    */
  protected[io] def parseBundleExpression(lines: List[String]): Option[State] = {
    @tailrec
    def f(sofar: Option[State], ls: List[String], rank: Int): Option[State] = (sofar, ls, rank) match {
      case (Some(st), x :: xs, _) if x.length == 2 + 3 * 9 && x.startsWith(s"P${rank}") =>
        f(parseOneLine(sofar, x.drop(2).grouped(3).toList, 9, rank), xs, rank + 1)
      case (_, Nil, 10) => sofar
      case _ => None
    }

    @tailrec
    def parseOneLine(sofar: Option[State], ls: List[String], file: Int, rank: Int): Option[State] = (sofar, ls, file) match {
      case (Some(st), x :: xs, _) if !List("+", "-").contains(x.take(1)) => // no piece
        parseOneLine(sofar, xs, file - 1, rank)
      case (Some(st), x :: xs, _) => // add one piece
        val pos = Square(file, rank)
        val p = Piece.parseCsaString(x)
        if (p.isDefined)
          parseOneLine(Some(State(st.turn, st.board + (pos -> p.get), st.hand)), xs, file - 1, rank)
        else
          None // invalid piece string
      case (_, Nil, 0) => sofar
      case _ => None
    }

    f(Some(State.empty), lines, 1).filter(_.checkCapacity)
  }

  protected[io] def parseSingleExpression(state: State, s: String): Option[(State, Boolean)] = {
    @tailrec
    def f(sofar: Option[(State, Boolean)], ls: List[String], t: Player): Option[(State, Boolean)] = (sofar, ls) match {
      case (Some((st, b)), "00AL" :: Nil) =>
        val rest = st.getUnusedPtypeCount.filter(_._1 != Ptype.KING)
        val newHands = MapUtil.mergeMaps(st.hand, rest.map { case (k, v) => Piece(t, k) -> v })(_ + _, 0)
        Some((State(st.turn, st.board, newHands), true))
      case (Some((st, b)), x :: xs) if x.length == 4 =>
        val next = for {
          pos <- Square.parseCsaString(x.substring(0, 2))
          pt <- Ptype.parseCsaString(x.substring(2, 4))
          if !st.board.contains(pos)
          if pos != Square.HAND || Ptype.inHand.contains(pt)
        } yield {
          val p = Piece(t, pt)
          pos match {
            case Square.HAND => (State(st.turn, st.board, st.hand.updated(p, st.hand.getOrElse(p, 0) + 1)), false)
            case _ => (State(st.turn, st.board.updated(pos, p), st.hand), false)
          }
        }
        f(next, xs, t)
      case (Some((st, b)), Nil) => if (st.checkCapacity) sofar else None
      case _ => None
    }

    if (s.startsWith("P+") || s.startsWith("P-")) {
      Player.parseCsaString(s.substring(1, 2)) flatMap { t => f(Some((state, false)), s.drop(2).grouped(4).toList, t) }
    } else {
      None
    }
  }

  def parseCsaString(s: String): Option[State] = {
    @tailrec
    def f(ss: List[String], sofar: Option[State], usedInit: Boolean, usedAll: Boolean): Option[State] = {
      (ss, sofar, usedInit, usedAll) match {
        case (x :: Nil, Some(st), _, _) => // Turn to move should be written in the last line
          Player.parseCsaString(x) map { t => State(t, st.board, st.hand) }
        case (x :: xs, Some(st), false, false) if x.startsWith("PI") => // Initialize with Hirate and handicap
          f(xs, parseInitExpression(x), usedInit = true, usedAll = false)
        case (x :: xs, Some(st), false, false) if x.startsWith("P1") =>
          f(ss.drop(9), parseBundleExpression(ss.take(9)), usedInit = true, usedAll = false)
        case (x :: xs, Some(st), _, false) if x.startsWith("P+") || x.startsWith("P-") =>
          parseSingleExpression(st, x) match {
            case Some((t, u)) => f(xs, Some(t), usedInit, u)
            case _ => None
          }
        case _ =>
          None
      }
    }

    f(s.split('\n').toList, Some(State.empty), usedInit = false, usedAll = false)
  }

}
