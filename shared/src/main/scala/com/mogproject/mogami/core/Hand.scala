package com.mogproject.mogami.core

/**
  * type-safe hand pieces
  */
case class Hand(owner: Player, ptype: Ptype) {
  require(ptype.isHandType, "piece must be an in-hand type")

  def toPiece: Piece = Piece(owner, ptype)

  def unary_! : Hand = Hand(!owner, ptype)
}

object Hand {
  implicit def ordering[A <: Hand]: Ordering[A] = Ordering.by(h => (h.owner, h.ptype))

  def apply(piece: Piece): Hand = Hand(piece.owner, piece.ptype)
}