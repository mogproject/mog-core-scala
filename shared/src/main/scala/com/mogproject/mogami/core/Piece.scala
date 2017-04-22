package com.mogproject.mogami.core

import com.mogproject.mogami.core.io._
import com.mogproject.mogami.core.io.sfen.{SfenLike, SfenTableFactory}
import com.mogproject.mogami.util.Implicits._

/**
  * Piece describing its owner and piece type
  */
case class Piece(owner: Player, ptype: Ptype) extends CsaLike with SfenLike with KifLike {
  def id: Int = owner.id * 16 + ptype.id

  override def toCsaString: String = Piece.csaTable(Piece.argsToId(owner, ptype))

  override def toSfenString: String = Piece.sfenTable(Piece.argsToId(owner, ptype))

  override def toKifString: String = Piece.kifTable(Piece.argsToId(owner, ptype))

  def unary_! : Piece = Piece(!owner, ptype)

  def promoted: Piece = Piece(owner, ptype.promoted)

  def demoted: Piece = Piece(owner, ptype.demoted)

  def isPromoted: Boolean = ptype.isPromoted

  def isRanged: Boolean = ptype.isRanged

  def canPromote: Boolean = ptype.canPromote
}

object Piece extends CsaTableFactory[Piece] with SfenTableFactory[Piece] with KifTableFactory[Piece] {
  override val typeName: String = "piece"

  implicit def ordering[A <: Piece]: Ordering[A] = Ordering.by(p => (p.owner, p.ptype))

  override val csaTable: Seq[String] = for {
    p <- Player.constructor
    pt <- Ptype.constructor
  } yield p.toCsaString + pt.toCsaString

  override val sfenTable: Seq[String] =
    (Ptype.englishSimpleNames ++ Ptype.englishSimpleNames.map(_.toLowerCase)).filter(_.nonEmpty)


  override val kifTable: Seq[String] = for {
    p <- Player.constructor
    pt <- Ptype.constructor
  } yield p.isBlack.fold(" ", "v") + pt.toJapaneseSimpleName

  def argsToId(owner: Player, ptype: Ptype): Int = owner.id * 14 + ptype.id - 2

  override def apply(id: Int): Piece = {
    assert(0 <= id && id < 28, s"Invalid id: ${id}")
    Piece(Player(id / 14), Ptype(id % 14 + 2))
  }
}