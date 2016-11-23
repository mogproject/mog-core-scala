package com.mogproject.mogami.core

import org.scalacheck.Gen

/**
  * Piece generator for scalacheck
  */
object PieceGen {
  val pieces: Gen[Piece] = for (p <- PlayerGen.players; pt <- PtypeGen.ptypes) yield Piece(p, pt)
}

