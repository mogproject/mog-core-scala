package com.mogproject.mogami.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import com.mogproject.mogami._
import com.mogproject.mogami.core.PieceConstant._

class HandSpec extends AnyFlatSpec with Matchers {

  "Hand#apply" must "create Hand instances" in {
    val h = Hand(BLACK, PAWN)
    h.owner mustBe BLACK
    h.ptype mustBe PAWN

    Hand(BLACK, PAWN) mustBe Hand(BP)
    Hand(BLACK, LANCE) mustBe Hand(BL)
    Hand(BLACK, KNIGHT) mustBe Hand(BN)
    Hand(BLACK, SILVER) mustBe Hand(BS)
    Hand(BLACK, GOLD) mustBe Hand(BG)
    Hand(BLACK, BISHOP) mustBe Hand(BB)
    Hand(BLACK, ROOK) mustBe Hand(BR)
    Hand(WHITE, PAWN) mustBe Hand(WP)
    Hand(WHITE, LANCE) mustBe Hand(WL)
    Hand(WHITE, KNIGHT) mustBe Hand(WN)
    Hand(WHITE, SILVER) mustBe Hand(WS)
    Hand(WHITE, GOLD) mustBe Hand(WG)
    Hand(WHITE, BISHOP) mustBe Hand(WB)
    Hand(WHITE, ROOK) mustBe Hand(WR)
  }
  it must "throw an error when the piece is not in-hand type" in {
    assertThrows[IllegalArgumentException](Hand(BLACK, KING))
    assertThrows[IllegalArgumentException](Hand(BLACK, PPAWN))
    assertThrows[IllegalArgumentException](Hand(BLACK, PLANCE))
    assertThrows[IllegalArgumentException](Hand(BLACK, PKNIGHT))
    assertThrows[IllegalArgumentException](Hand(BLACK, PSILVER))
    assertThrows[IllegalArgumentException](Hand(BLACK, PBISHOP))
    assertThrows[IllegalArgumentException](Hand(BLACK, PROOK))
    assertThrows[IllegalArgumentException](Hand(WHITE, KING))
    assertThrows[IllegalArgumentException](Hand(WHITE, PPAWN))
    assertThrows[IllegalArgumentException](Hand(WHITE, PLANCE))
    assertThrows[IllegalArgumentException](Hand(WHITE, PKNIGHT))
    assertThrows[IllegalArgumentException](Hand(WHITE, PSILVER))
    assertThrows[IllegalArgumentException](Hand(WHITE, PBISHOP))
    assertThrows[IllegalArgumentException](Hand(WHITE, PROOK))
  }

  "Hand#toPiece" must "return Piece instances" in {
    Hand(BLACK, PAWN).toPiece mustBe BP
    Hand(WHITE, ROOK).toPiece mustBe WR
  }

  "Hand" must "be able to sort" in {
    List(Hand(BP), Hand(WP), Hand(BR)).sorted mustBe List(Hand(BR), Hand(BP), Hand(WP))
  }

  "Hand#unary_!" must "flip the owner" in {
    !Hand(BLACK, PAWN) mustBe Hand(WHITE, PAWN)
    !Hand(WHITE, PAWN) mustBe Hand(BLACK, PAWN)
  }

}
