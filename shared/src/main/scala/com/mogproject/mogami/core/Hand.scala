package com.mogproject.mogami.core

import com.mogproject.mogami.core.io.{CsaLike, SfenLike}

/**
  * type-safe hand pieces
  */
//case class Hand(owner: Player, ptype: Ptype) extends CsaLike with SfenLike {
case class Hand(owner: Player, ptype: Ptype) {
  require(ptype.isHandType, "piece must be an in-hand type")

  def toPiece: Piece = Piece(owner, ptype)

//  override def toCsaString: String = toPiece.toCsaString

//  override def toSfenString: String = toPiece.toSfenString
}

object Hand {
  implicit def ordering[A <: Hand]: Ordering[A] = Ordering.by(h => (h.owner, h.ptype))

  def apply(piece: Piece): Hand = Hand(piece.owner, piece.ptype)
}