package com.mogproject.mogami.core

import org.scalacheck.Gen

object BitBoardGen {

  // BitBoard generator for scalacheck
  val bitboards: Gen[BitBoard] =
    for (lo <- Gen.choose(0L, 1L << 54 - 1L); hi <- Gen.choose(0L, 1L << 27 - 1L)) yield BitBoard(lo, hi)
}
