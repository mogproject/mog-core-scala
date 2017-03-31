package com.mogproject.mogami.core

import com.mogproject.mogami.core.io._
import com.mogproject.mogami.util.Implicits._

/**
  * Player
  */
sealed abstract class Player(val id: Int) extends CsaLike with SfenLike {
  def unary_! : Player = Player(id ^ 1)

  def isBlack: Boolean = id == 0

  def isWhite: Boolean = id == 1

  override def toCsaString: String = Player.csaTable(id)

  override def toSfenString: String = Player.sfenTable(id)

  def toSymbolString(unicode: Boolean = true): String = unicode.fold(Player.symbolTableUnicode(id), Player.symbolTable(id))

  def toJapaneseNotationString(handicap: Boolean = false): String = handicap.fold(Player.japaneseNotationTableHandicap(id), Player.japaneseNotationTable(id))
}

object Player extends CsaTableFactory[Player] with SfenTableFactory[Player] {
  implicit def ordering[A <: Player]: Ordering[A] = Ordering.by(_.id)

  override val csaTable: Seq[String] = Seq("+", "-")

  override val sfenTable: Seq[String] = Seq("b", "w")

  val symbolTable = Seq("▲", "△")

  val symbolTableUnicode = Seq("☗", "☖")

  val japaneseNotationTable = Seq("先手", "後手")

  val japaneseNotationTableHandicap = Seq("下手", "上手")

  val constructor: Seq[Player] = Seq(BLACK, WHITE)

  def apply(id: Int): Player = {
    assert(0 <= id && id < 2)
    constructor(id)
  }

  case object BLACK extends Player(0)

  case object WHITE extends Player(1)

}
