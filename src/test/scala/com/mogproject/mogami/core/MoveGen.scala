package com.mogproject.mogami.core

import org.scalacheck.Gen

/**
  * Move generator for scalacheck
  */
object MoveGen {
  val movesCsaFormat: Gen[Move] = for {
    from <- SquareGen.squares
    to <- SquareGen.squaresOnBoard
    pl <- PlayerGen.players
    pt <- if (from.isHand) PtypeGen.ptypesInHand else PtypeGen.ptypes
  } yield Move(from, to, Some(pl), Some(pt), None)

  val movesSfenFormat: Gen[Move] = for {
    from <- SquareGen.squares
    to <- SquareGen.squaresOnBoard
    pt <- PtypeGen.ptypesInHand
    pr <- Gen.oneOf(false, true)
  } yield {
    if (from.isHand)
      Move(from, to, None, Some(pt), Some(false))
    else
      Move(from, to, None, None, Some(pr))
  }
}

