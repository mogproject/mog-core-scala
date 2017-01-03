package com.mogproject.mogami.core

import org.scalacheck.Gen
import com.mogproject.mogami.util.Implicits._

/**
  * Move generator for scalacheck
  */
object MoveGen {
  val movesCsaFormat: Gen[MoveBuilderCsa] = for {
    isHand <- Gen.oneOf(true, false, false, false) // a chance of 1/4
    from <- SquareGen.squares
    to <- SquareGen.squaresOnBoardExcept(Seq(from))
    pl <- PlayerGen.players
    pt <- if (isHand) PtypeGen.ptypesInHand else PtypeGen.ptypes
    t <- Gen.option(Gen.choose(0, 1000000000))
  } yield isHand.fold(MoveBuilderCsaHand(pl, to, pt, t), MoveBuilderCsaBoard(pl, from, to, pt, t))

  val movesSfenFormat: Gen[MoveBuilderSfen] = for {
    isHand <- Gen.oneOf(true, false, false, false)
    from <- SquareGen.squares
    to <- SquareGen.squaresOnBoardExcept(Seq(from))
    pt <- PtypeGen.ptypesInHand
    pr <- Gen.oneOf(false, true)
  } yield isHand.fold(MoveBuilderSfenHand(pt, to), MoveBuilderSfenBoard(from, to, pr))
}

