package com.mogproject.mogami.core

import com.mogproject.mogami.core.io.{CsaLike, CsaTableFactory}

/**
  * Piece type
  */
sealed abstract class Ptype(val id: Int) extends CsaLike {
  lazy val sortId: Int = Ptype.sortIdTable(id)

  override def toCsaString: String = Ptype.csaTable(id)

  final def isBasic: Boolean = 8 <= id

  final def isPromoted: Boolean = id < 8

  final def canPromote: Boolean = 10 <= id

  final def isRanged: Boolean = id == 11 || (id & 7) >= 6

  final def demoted: Ptype = Ptype(id | 8)

  final def promoted: Ptype = if (canPromote) Ptype(id - 8) else this

  lazy val toEnglishSimpleName: String = Ptype.englishSimpleNames(id)

  lazy val toJapaneseSimpleName: String = Ptype.japaneseSimpleNames(id)

  val capability: Vector[Int] = Vector(
    Vector(0, 0, 0, 0, 0, 0, -1),
    Vector(0, 0, 0, 0, 0, 0, -1),
    Vector(1, 1, 1, 0, 1, 0, -1), // PPAWN
    Vector(1, 1, 1, 0, 1, 0, -1), // PLANCE
    Vector(1, 1, 1, 0, 1, 0, -1), // PKNIGHT
    Vector(1, 1, 1, 0, 1, 0, -1), // PSILVER
    Vector(1, 8, 1, 8, 1, 0, -1), // PBISHOP
    Vector(8, 1, 8, 1, 8, 0, -1), // PROOK
    Vector(1, 1, 1, 1, 1, 0, -1), // KING
    Vector(1, 1, 1, 0, 1, 0, -1), // GOLD
    Vector(1, 0, 0, 0, 0, 0, -1), // PAWN
    Vector(8, 0, 0, 0, 0, 0, -1), // LANCE
    Vector(0, 0, 0, 0, 0, 1, -1), // KNIGHT
    Vector(1, 1, 0, 1, 0, 0, -1), // SILVER
    Vector(0, 8, 0, 8, 0, 0, -1), // BISHOP
    Vector(8, 0, 8, 0, 8, 0, -1) // ROOK
  )(id)

  def canMoveTo(displacement: Displacement): Boolean = displacement.distance <= capability(displacement.direction.id)
}

object Ptype extends CsaTableFactory[Ptype] {
  implicit def ordering[A <: Ptype]: Ordering[A] = Ordering.by(_.sortId)

  val sortIdTable: Seq[Int] = Seq(
    -1, -1, 15, 14, 13, 12, 10, 9,
    0, 3, 7, 6, 5, 4, 2, 1
  )

  val csaTable: Seq[String] = Seq(
    "", "", "TO", "NY", "NK", "NG", "UM", "RY",
    "OU", "KI", "FU", "KY", "KE", "GI", "KA", "HI"
  )

  val englishSimpleNames: Seq[String] = Seq(
    "", "", "+P", "+L", "+N", "+S", "+B", "+R",
    "K", "G", "P", "L", "N", "S", "B", "R"
  )

  val japaneseSimpleNames: Seq[String] = Seq(
    "", "", "と", "杏", "圭", "全", "馬", "竜",
    "玉", "金", "歩", "香", "桂", "銀", "角", "飛"
  )

  val constructor: Seq[Ptype] = Seq(
    PPAWN, PLANCE, PKNIGHT, PSILVER, PBISHOP, PROOK,
    KING, GOLD, PAWN, LANCE, KNIGHT, SILVER, BISHOP, ROOK
  )

  val inHand: Seq[Ptype] = Seq(
    ROOK, BISHOP, GOLD, SILVER, KNIGHT, LANCE, PAWN
  )

  def apply(id: Int): Ptype = {
    assert(2 <= id && id < 16, s"Invalid id: ${id}")
    constructor(id - 2)
  }

  case object PPAWN extends Ptype(2)

  case object PLANCE extends Ptype(3)

  case object PKNIGHT extends Ptype(4)

  case object PSILVER extends Ptype(5)

  case object PBISHOP extends Ptype(6)

  case object PROOK extends Ptype(7)

  case object KING extends Ptype(8)

  case object GOLD extends Ptype(9)

  case object PAWN extends Ptype(10)

  case object LANCE extends Ptype(11)

  case object KNIGHT extends Ptype(12)

  case object SILVER extends Ptype(13)

  case object BISHOP extends Ptype(14)

  case object ROOK extends Ptype(15)

}
