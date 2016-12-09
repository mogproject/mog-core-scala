package com.mogproject.mogami.core

import com.mogproject.mogami.core.io._

/**
  * Piece describing its owner and piece type
  */
case class Piece(owner: Player, ptype: Ptype) extends CsaLike with SfenLike {
  override def toCsaString = Piece.csaTable(Piece.argsToId(owner, ptype))

  override def toSfenString = Piece.sfenTable(Piece.argsToId(owner, ptype))

  def unary_! : Piece = Piece(!owner, ptype)

  def promoted: Piece = Piece(owner, ptype.promoted)

  def demoted: Piece = Piece(owner, ptype.demoted)
}

object Piece extends CsaTableFactory[Piece] with SfenTableFactory[Piece] {
  override val csaTable: Seq[String] = for {
    p <- Player.constructor
    pt <- Ptype.constructor
  } yield p.toCsaString + pt.toCsaString

  override val sfenTable: Seq[String] =
    (Ptype.englishSimpleNames ++ Ptype.englishSimpleNames.map(_.toLowerCase)).filter(_.nonEmpty)

  def argsToId(owner: Player, ptype: Ptype): Int = owner.id * 14 + ptype.id - 2

  override def apply(id: Int): Piece = {
    assert(0 <= id && id < 28, s"Invalid id: ${id}")
    Piece(Player(id / 14), Ptype(id % 14 + 2))
  }
}