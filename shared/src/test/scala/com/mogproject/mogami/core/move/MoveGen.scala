package com.mogproject.mogami.core.move

import com.mogproject.mogami.core.{PlayerGen, PtypeGen, SquareGen}
import com.mogproject.mogami.util.Implicits._
import org.scalacheck.Gen

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
    pt <- if (isHand) PtypeGen.ptypesInHand else PtypeGen.ptypes
    pr <- Gen.oneOf(false, !pt.isPromoted)
  } yield isHand.fold(MoveBuilderSfenHand(pt, to), MoveBuilderSfenBoard(from, to, pr))

  val movesKifFormat: Gen[MoveBuilderKif] = for {
    isHand <- Gen.oneOf(true, false, false, false)
    from <- SquareGen.squares
    to <- SquareGen.squaresOnBoardExcept(Seq(from))
    pt <- if (isHand) PtypeGen.ptypesInHand else PtypeGen.ptypes
    pr <- Gen.oneOf(false, !pt.isPromoted)
    t <- Gen.option(Gen.choose(0, 1000000000))
  } yield isHand.fold(MoveBuilderKifHand(to, pt, t), MoveBuilderKifBoard(from, Some(to), pt, pr, t))

  val movesKi2Format: Gen[MoveBuilderKi2] = for {
    pl <- PlayerGen.players
    isHand <- Gen.oneOf(true, false, false, false)
    to <- SquareGen.squares
    pt <- if (isHand) PtypeGen.ptypesInHand else PtypeGen.ptypes
    canPromote = pt.canPromote && !isHand
    pr <- Gen.oneOf(None, canPromote.fold(Some(true), None), canPromote.fold(Some(false), None))
    mvmt <- Gen.option(Gen.oneOf(Movement.all))
  } yield MoveBuilderKi2(pl, isHand.fold(None, Some(to)), pt, mvmt, pr)

}

