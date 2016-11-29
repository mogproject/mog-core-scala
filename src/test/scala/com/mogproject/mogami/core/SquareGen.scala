package com.mogproject.mogami.core

import com.mogproject.mogami.core.Square.HAND
import org.scalacheck.Gen

/**
  * Square generator for scalacheck
  */
object SquareGen {
  private val boards = for {f <- 1 to 9; r <- 1 to 9} yield Square(f, r)

  val squares: Gen[Square] = Gen.oneOf(HAND +: boards)

  val squaresOnBoard: Gen[Square] = Gen.oneOf(boards)
}
