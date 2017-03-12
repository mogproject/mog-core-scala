package com.mogproject.mogami.core.move

import com.mogproject.mogami._
import com.mogproject.mogami.core.move.Movement._
import com.mogproject.mogami.core.io._
import com.mogproject.mogami.util.Implicits._


/**
  * Move with complete information
  */
case class Move(player: Player,
                from: Option[Square], // None if from hand
                to: Square,
                newPtype: Ptype,
                promote: Boolean,
                isSameSquare: Boolean, // true when `to` is same as the last move's `to`
                movement: Option[Movement], // None = no ambiguity
                captured: Option[Ptype],
                isCheck: Boolean,
                elapsedTime: Option[Int] = None
               ) extends CsaLike with SfenLike with KifLike {
  require(!isDrop || !promote, "promote must be false when dropping")
  require(!isDrop || captured.isEmpty, "captured must be None when dropping")
  require(from.exists(_.isPromotionZone(player)) || to.isPromotionZone(player) || !promote, "either from or to must be in the promotion zone")
  require(from.map(_.getDisplacement(player, to)).forall(oldPtype.canMoveTo), "move must be within the capability")
  require(to.isLegalZone(newPiece), "to must be legal for the new piece")
  require(elapsedTime.forall(_ >= 0), "elapsedTime must be positive or zero")
  require(!captured.contains(KING), "king cannot be captured")
  require(oldPtype != PAWN || movement.isEmpty, "pawn cannot be ambiguous")

  def oldPtype: Ptype = if (promote) newPtype.demoted else newPtype

  def oldPiece: Piece = Piece(player, oldPtype)

  def newPiece: Piece = Piece(player, newPtype)

  def isDrop: Boolean = from.isEmpty

  def isAmbiguous: Boolean = movement.isDefined

  def hasCapture: Boolean = captured.isDefined

  def capturedPiece: Option[Piece] = captured.map(Piece(!player, _))

  def moveFrom: MoveFrom = from.map(Left.apply).getOrElse(Right(Hand(player, oldPtype)))

  def couldPromote: Boolean = oldPtype.canPromote && from.exists(_.isPromotionZone(player) || to.isPromotionZone(player))

  override def toCsaString: String =
    from.map(fr => MoveBuilderCsaBoard(player, fr, to, newPtype, elapsedTime))
      .getOrElse(MoveBuilderCsaHand(player, to, newPtype, elapsedTime)).toCsaString

  override def toSfenString: String =
    from.map(fr => MoveBuilderSfenBoard(fr, to, promote)).getOrElse(MoveBuilderSfenHand(newPtype, to)).toSfenString

  override def toKifString: String = {
    val mv = isSameSquare.fold("同　", to.toKifString) + oldPtype.toKifString + isDrop.fold("打", promote.fold("成", "") + s"(${from.map(_.toCsaString).getOrElse("")})")
    val tm = elapsedTime.map { t => f" (${t / 60}%02d:${t % 60}%02d/)" }.getOrElse("")
    mv + tm
  }

  def toJapaneseNotationString: String =
    isSameSquare.fold("同", to.toKifString) + oldPtype.toKifString + movement.map(_.kifString).getOrElse("") + promote.fold("成", couldPromote.fold("不成", ""))

  def toWesternNotationString: String = {
    val movementType = isDrop.fold("*", hasCapture.fold("x", "-"))
    val origin = isAmbiguous.fold(from.map(_.toSfenString).getOrElse(""), "")
    val promotionStatus = promote.fold("+", couldPromote.fold("=", ""))
    s"${oldPtype.toEnglishSimpleName}${origin}${movementType}${to.toSfenString}${promotionStatus}"
  }
}
