package com.mogproject.mogami.core

import org.scalacheck.Gen

/**
  * Player generator for scalacheck
  */
object PlayerGen {
  val players: Gen[Player] = Gen.oneOf(Player.constructor)
}
