package com.mogproject.mogami.core.move


import com.mogproject.mogami._
import com.mogproject.mogami.core.io._

import scala.util.Try
import scala.util.matching.Regex


sealed trait MoveBuilderCsa extends MoveBuilder with CsaLike

object MoveBuilderCsa extends CsaFactory[MoveBuilderCsa] {
  private[this] val pattern: Regex = """([^,]+)(?:,T([0-9]+))?""".r

  def parseTime(s: String): Option[(String, Option[Int])] = s match {
    case pattern(mv, null) => Some((mv, None))
    case pattern(mv, tm) => Try(tm.toInt).filter(_ >= 0).map(x => (mv, Some(x))).toOption
    case _ => None
  }

  override def parseCsaString(s: String): Option[MoveBuilderCsa] = {
    val isHand = s.slice(1, 3) == "00"
    for {
      (mv, t) <- parseTime(s)
      pl <- Player.parseCsaString(mv.substring(0, 1))
      from <- if (isHand) Some(Square(0)) else Square.parseCsaString(mv.substring(1, 3))
      to <- Square.parseCsaString(mv.substring(3, 5)) if isHand || to != from
      pt <- Ptype.parseCsaString(mv.substring(5)) if !isHand || pt.isHandType
    } yield {
      if (isHand) MoveBuilderCsaHand(pl, to, pt, t) else MoveBuilderCsaBoard(pl, from, to, pt, t)
    }
  }
}

case class MoveBuilderCsaBoard(player: Player, from: Square, to: Square, newPtype: Ptype, elapsedTime: Option[Int] = None) extends MoveBuilderCsa {
  override def toCsaString: String = List(player, from, to, newPtype).map(_.toCsaString).mkString + timeToCsaString(elapsedTime)

  override def toMove(state: State, isStrict: Boolean = true): Option[Move] =
    for {
      oldPiece <- state.board.get(from)
      promote = oldPiece.ptype != newPtype
      isSame = state.lastMoveTo.contains(to)
      isCheck = isCheckMove(state, Some(from), to, newPtype)
      movement = getMovement(state, Some(from), to, oldPiece.ptype)
      captured = state.board.get(to).map(_.ptype).filter(_ != KING)
      mv <- Try(Move(player, Some(from), to, newPtype, promote, isSame, movement, captured, isCheck, elapsedTime, isStrict)).toOption
      if player == state.turn
    } yield mv
}

case class MoveBuilderCsaHand(player: Player, to: Square, ptype: Ptype, elapsedTime: Option[Int] = None) extends MoveBuilderCsa {
  override def toCsaString: String = s"${player.toCsaString}00${to.toCsaString}${ptype.toCsaString}${timeToCsaString(elapsedTime)}"

  override def toMove(state: State, isStrict: Boolean = true): Option[Move] = {
    val isCheck = isCheckMove(state, None, to, ptype)
    val movement = getMovement(state, None, to, ptype)
    for {
      mv <- Try(Move(player, None, to, ptype, promote = false, isSameSquare = false, movement, captured = None, isCheck, elapsedTime, isStrict)).toOption
      if player == state.turn
    } yield mv
  }
}
