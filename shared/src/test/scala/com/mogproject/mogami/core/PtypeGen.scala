package com.mogproject.mogami.core

import org.scalacheck.Gen

/**
  * Piece type generator for scalacheck
  */
object PtypeGen {
  val ptypes: Gen[Ptype] = Gen.oneOf(Ptype.constructor)

  val ptypesInHand: Gen[Ptype] = Gen.oneOf(Ptype.inHand)
}
