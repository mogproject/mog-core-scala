package com.mogproject.mogami.core

import com.mogproject.mogami._
import com.mogproject.mogami.core.io._
import com.mogproject.mogami.util.Implicits._

import scala.util.Try
import scala.util.matching.Regex

/**
  * Move class
  */
case class Move(from: Square,
                to: Square,
                player: Option[Player],
                newPtype: Option[Ptype],
                promote: Option[Boolean],
                elapsedTime: Option[Int] = None
               ) extends CsaLike with SfenLike {
  require(!from.isHand || newPtype.exists(Ptype.inHand.contains), "ptype must be defined and in-hand type when dropping")
  require(!from.isHand || !promote.contains(true), "promote must be undefined or false when dropping")
  require(from != to, "to must not be identical to from")
  require(!to.isHand, "to must not be in hand")
  require(newPtype.isDefined || promote.isDefined, "either ptype or promote must be defined")
  require(elapsedTime.forall(_ >= 0), "elapsedTime must be positive or zero")

  def isCsaCompatible: Boolean = player.isDefined && newPtype.isDefined

  def isSfenCompatible: Boolean = promote.isDefined

  override def toCsaString: String = {
    require(isCsaCompatible)
    List(player.get, from, to, newPtype.get).map(_.toCsaString).mkString + elapsedTime.map(x => s",T${x}").getOrElse("")
  }

  override def toSfenString: String = {
    require(isSfenCompatible)
    if (from.isHand) {
      List(Piece(Player.BLACK, newPtype.get), from, to).map(_.toSfenString).mkString
    } else {
      s"${from.toSfenString}${to.toSfenString}" + (if (promote.get) "+" else "")
    }
  }
}

object Move extends CsaFactory[Move] with SfenFactory[Move] {
  def parseCsaString(s: String): Option[Move] = {
    val r: Regex = """(.{7})(?:,T([0-9]+))?""".r

    val result: Option[(String, Option[Int])] = s match {
      case r(mv, null) => Some(mv, None)
      case r(mv, tm) => Try(tm.toInt).map(x => (mv, Some(x))).toOption
      case _ => None
    }

    for {
      (mv, t) <- result
      pl <- Player.parseCsaString(mv.substring(0, 1))
      from <- Square.parseCsaString(mv.substring(1, 3))
      to <- Square.parseCsaString(mv.substring(3, 5)) if to != Square.HAND && to != from
      pt <- Ptype.parseCsaString(mv.substring(5))
      if t.forall(_ >= 0)
    } yield {
      Move(from, to, Some(pl), Some(pt), None, t)
    }
  }

  def parseSfenString(s: String): Option[Move] = {
    val patternOnBoard: Regex = """([1-9][a-i])([1-9][a-i])([+]?)""".r
    val patternInHand: Regex = """([PLNSGBR])[*]([1-9][a-i])""".r
    s match {
      case patternOnBoard(from, to, promote) =>
        for {
          f <- Square.parseSfenString(from)
          t <- Square.parseSfenString(to) if to != from
        } yield Move(f, t, None, None, Some(promote == "+"))
      case patternInHand(ptype, to) =>
        for {
          pt <- Piece.parseSfenString(ptype)
          t <- Square.parseSfenString(to)
        } yield Move(Square.HAND, t, None, Some(pt.ptype), Some(false))
      case _ => None
    }
  }
}


/**
  * Move with complete information
  */
case class ExtendedMove(player: Player,
                        from: Square,
                        to: Square,
                        newPtype: Ptype,
                        promote: Boolean,
                        captured: Option[Ptype],
                        isCheck: Boolean,
                        elapsedTime: Option[Int] = None
                       ) extends CsaLike with SfenLike {
  require(!isDrop || !promote, "promote must be false when dropping")
  require(!isDrop || captured.isEmpty, "captured must be None when dropping")
  require(from.isPromotionZone(player) || to.isPromotionZone(player) || !promote, "either from or to must be in the promotion zone")
  require(from != to, "to must not be identical to from")
  require(!to.isHand, "to must not be in hand")
  require(isDrop || oldPtype.canMoveTo(from.getDisplacement(player, to)), "move must be within the capability")
  require(to.isLegalZone(newPiece), "to must be legal for the new piece")
  require(elapsedTime.forall(_ >= 0), "elapsedTime must be positive or zero")
  require(!captured.contains(KING), "king cannot be captured")

  def oldPtype: Ptype = if (promote) newPtype.demoted else newPtype

  def oldPiece: Piece = Piece(player, oldPtype)

  def newPiece: Piece = Piece(player, newPtype)

  def isDrop: Boolean = from.isHand

  def hasCapture: Boolean = captured.isDefined

  def capturedPiece: Option[Piece] = captured.map(Piece(!player, _))

  private[this] def toMove: Move = Move(from, to, Some(player), Some(newPtype), Some(promote))

  override def toCsaString: String = toMove.toCsaString + elapsedTime.map(x => s",T${x}").getOrElse("")

  override def toSfenString: String = toMove.toSfenString

}

object ExtendedMove {

  /**
    * Complete move information with a state instance
    *
    * @param move move instance
    * @return completed information
    */
  def fromMove(move: Move, state: State): Option[ExtendedMove] = {
    for {
      oldPiece <- if (move.from.isHand) Some(Piece(state.turn, move.newPtype.get)) else state.board.get(move.from)
      oldPtype = oldPiece.ptype
      newPtype = move.newPtype.getOrElse(if (move.promote.get) oldPtype.promoted else oldPtype)
      promote = move.promote.getOrElse(oldPtype != newPtype)
      isCheck = isCheckMove(Piece(state.turn, newPtype), move.from, move.to, state)
      captured = state.board.get(move.to).map(_.ptype)
      mv <- Try(ExtendedMove(state.turn, move.from, move.to, newPtype, promote, captured, isCheck, move.elapsedTime)).toOption
    } yield mv
  }

  def isCheckMove(newPiece: Piece, from: Square, to: Square, state: State): Boolean = {
    val pl = state.turn
    val newOccAll = (!from.isHand).when[BitBoard](_.reset(from))(state.occupancy.set(to))
    val king = state.getKing(!pl)

    king.exists { k =>
      val pieces = (to, newPiece) +: state.getRangedPieces(pl)
      // `to` must not be in hand, and therefore Nifu does not matter
      pieces.exists { case (s, p) => Attack.get(p, s, newOccAll, BitBoard.empty).get(k) }
    }
  }
}