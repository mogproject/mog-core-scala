package com.mogproject.mogami.core

import com.mogproject.mogami.core.io.{CsaFactory, CsaLike}

/**
  * Piece type
  */
sealed abstract class Ptype(val id: Int) extends CsaLike {
  override def toCsaString: String = Ptype.csaTable(id)

  final def isBasic: Boolean = 8 <= id

  final def isPromoted: Boolean = id < 8

  final def canPromote: Boolean = 10 <= id

  final def demoted: Ptype = Ptype(id | 8)

  final def promoted: Ptype = if (canPromote) Ptype(id - 8) else this

  def toEnglishSimpleName: String = Ptype.englishSimpleNames(id)

  def toJapaneseSimpleName: String = Ptype.japaneseSimpleNames(id)
}

object Ptype extends CsaFactory[Ptype] {
  override val csaTable: Seq[String] = Seq(
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
