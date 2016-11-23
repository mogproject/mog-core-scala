package com.mogproject.mogami.core

import com.mogproject.mogami.core.io.{CsaFactory, CsaLike, SfenFactory, SfenLike}

/**
  * Player
  */
sealed abstract class Player(val id: Int) extends CsaLike with SfenLike {
  def unary_! : Player = Player(id ^ 1)

  override def toCsaString: String = Player.csaTable(id)

  override def toSfenString: String = Player.sfenTable(id)
}

object Player extends CsaFactory[Player] with SfenFactory[Player] {
  override val csaTable: Seq[String] = Seq("+", "-")

  override val sfenTable: Seq[String] = Seq("b", "w")

  val constructor: Seq[Player] = Seq(BLACK, WHITE)

  def apply(id: Int): Player = constructor(id)

  case object BLACK extends Player(0)

  case object WHITE extends Player(1)
}
