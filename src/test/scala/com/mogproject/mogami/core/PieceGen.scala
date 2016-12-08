package com.mogproject.mogami.core

import com.mogproject.mogami.core.Player.{BLACK, WHITE}
import com.mogproject.mogami.core.Ptype._
import org.scalacheck.Gen

/**
  * Piece generator for scalacheck
  */
object PieceGen {
  val pieces: Gen[Piece] = for (p <- PlayerGen.players; pt <- PtypeGen.ptypes) yield Piece(p, pt)
}

/**
  * Constant values
  */
object PieceConstant {
  val BP = Piece(BLACK, PAWN)
  val BL = Piece(BLACK, LANCE)
  val BN = Piece(BLACK, KNIGHT)
  val BS = Piece(BLACK, SILVER)
  val BG = Piece(BLACK, GOLD)
  val BB = Piece(BLACK, BISHOP)
  val BR = Piece(BLACK, ROOK)
  val BK = Piece(BLACK, KING)
  val BPP = Piece(BLACK, PPAWN)
  val BPL = Piece(BLACK, PLANCE)
  val BPN = Piece(BLACK, PKNIGHT)
  val BPS = Piece(BLACK, PSILVER)
  val BPB = Piece(BLACK, PBISHOP)
  val BPR = Piece(BLACK, PROOK)
  val WP =  Piece(WHITE, PAWN)
  val WL =  Piece(WHITE, LANCE)
  val WN =  Piece(WHITE, KNIGHT)
  val WS =  Piece(WHITE, SILVER)
  val WG =  Piece(WHITE, GOLD)
  val WB =  Piece(WHITE, BISHOP)
  val WR =  Piece(WHITE, ROOK)
  val WK =  Piece(WHITE, KING)
  val WPP = Piece(WHITE, PPAWN)
  val WPL = Piece(WHITE, PLANCE)
  val WPN = Piece(WHITE, PKNIGHT)
  val WPS = Piece(WHITE, PSILVER)
  val WPB = Piece(WHITE, PBISHOP)
  val WPR = Piece(WHITE, PROOK)
}