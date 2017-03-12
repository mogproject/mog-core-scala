package com.mogproject.mogami.core.move

import com.mogproject.mogami._
import com.mogproject.mogami.core.io._
import com.mogproject.mogami.util.Implicits._

import scala.util.Try
import scala.util.matching.Regex

/**
  *
  */

/**
  * Move builder for SFEN-formatted string
  */
sealed trait MoveBuilderSfen extends MoveBuilder with SfenLike {
}

object MoveBuilderSfen extends SfenFactory[MoveBuilderSfen] {
  private[this] val patternOnBoard: Regex = """([1-9][a-i])([1-9][a-i])([+]?)""".r
  private[this] val patternInHand: Regex = """([PLNSGBR])[*]([1-9][a-i])""".r

  override def parseSfenString(s: String): Option[MoveBuilderSfen] = {
    s match {
      case patternOnBoard(from, to, promote) =>
        for {
          f <- Square.parseSfenString(from)
          t <- Square.parseSfenString(to) if to != from
        } yield MoveBuilderSfenBoard(f, t, promote == "+")
      case patternInHand(ptype, to) =>
        for {
          p <- Piece.parseSfenString(ptype) // already assured that ptype is in-hand type
          t <- Square.parseSfenString(to)
        } yield MoveBuilderSfenHand(p.ptype, t)
      case _ => None
    }
  }

  def apply(from: MoveFrom, to: Square, promote: Boolean): MoveBuilderSfen = from match {
    case Left(sq) =>
      MoveBuilderSfenBoard(sq, to, promote)
    case Right(h) =>
      require(!promote, "promote must be false when dropping")
      MoveBuilderSfenHand(h.ptype, to)
  }
}

case class MoveBuilderSfenBoard(from: Square, to: Square, promote: Boolean) extends MoveBuilderSfen {
  override def toSfenString: String = s"${from.toSfenString}${to.toSfenString}${promote.fold("+", "")}"

  override def toMove(state: State): Option[Move] =
    for {
      oldPiece <- state.board.get(from)
      newPtype = promote.fold(oldPiece.ptype.promoted, oldPiece.ptype)
      isSame = state.lastMoveTo.contains(to)
      isCheck = isCheckMove(state, Some(from), to, newPtype)
      movement = getMovement(state, Some(from), to, oldPiece.ptype)
      captured = state.board.get(to).map(_.ptype)
      mv <- Try(Move(state.turn, Some(from), to, newPtype, promote, isSame, movement, captured, isCheck, None)).toOption
    } yield mv
}

case class MoveBuilderSfenHand(ptype: Ptype, to: Square) extends MoveBuilderSfen {
  override def toSfenString: String = s"${Piece(Player.BLACK, ptype).toSfenString}*${to.toSfenString}"

  override def toMove(state: State): Option[Move] = {
    val isCheck = isCheckMove(state, None, to, ptype)
    val movement = getMovement(state, None, to, ptype)
    for {
      mv <- Try(Move(state.turn, None, to, ptype, promote = false, isSameSquare = false, movement, None, isCheck, None)).toOption
    } yield mv
  }
}
