package com.mogproject.mogami.core

import com.mogproject.mogami.core.Player.{BLACK, WHITE}
import com.mogproject.mogami.core.Ptype.{KNIGHT, LANCE, PAWN, SILVER}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import com.mogproject.mogami.core.SquareConstant._
import com.mogproject.mogami.core.Direction._
import com.mogproject.mogami.core.io.RecordFormatException

class SquareSpec extends AnyFlatSpec with Matchers with ScalaCheckDrivenPropertyChecks {
  private val csaSquare = for (r <- '1' to '9'; f <- '1' to '9') yield s"$f$r"
  private val sfenSquare = for (r <- 'a' to 'i'; f <- '1' to '9') yield s"$f$r"
  private val kifSquare = for (r <- "一二三四五六七八九"; f <- "１２３４５６７８９") yield s"$f$r"

  "Square#unary_!" must "flip the position" in {
    !P11 mustBe P99
    !P12 mustBe P98
    !P19 mustBe P91
    !P55 mustBe P55
    !P76 mustBe P34
    !P99 mustBe P11
  }
  it must "cancel double negation" in forAll(SquareGen.squares) { sq =>
    !(!sq) must be(sq)
  }

  "Square#parseCsaString" must "return Some in normal cases" in {
    csaSquare map { c => Square.parseCsaString(c) } mustBe Square.all
  }
  it must "throw an exception in error cases" in {
    assertThrows[RecordFormatException](Square.parseCsaString(""))
    assertThrows[RecordFormatException](Square.parseCsaString(" "))
    assertThrows[RecordFormatException](Square.parseCsaString("x" * 1000))
    assertThrows[RecordFormatException](Square.parseCsaString("01"))
    assertThrows[RecordFormatException](Square.parseCsaString("90"))
    assertThrows[RecordFormatException](Square.parseCsaString("123"))
  }
  "Square#toCsaString" must "make CSA-formatted string" in {
    Square.all map (_.toCsaString) mustBe csaSquare
  }
  it must "recover the original square" in forAll(SquareGen.squares) { s =>
    Square.parseCsaString(s.toCsaString) mustBe s
  }
  "Square#parseSfenString" must "return Some in normal cases" in {
    sfenSquare map { c => Square.parseSfenString(c) } mustBe Square.all
  }
  it must "return None in error cases" in {
    assertThrows[RecordFormatException](Square.parseSfenString(""))
    assertThrows[RecordFormatException](Square.parseSfenString(" "))
    assertThrows[RecordFormatException](Square.parseSfenString("x" * 1000))
    assertThrows[RecordFormatException](Square.parseSfenString("0a"))
    assertThrows[RecordFormatException](Square.parseSfenString("i0"))
    assertThrows[RecordFormatException](Square.parseSfenString("123"))
  }
  "Square#toSfenString" must "make SFEN-formatted string" in {
    Square.all map (_.toSfenString) mustBe sfenSquare
  }
  it must "recover the original square" in forAll(SquareGen.squares) { s =>
    Square.parseSfenString(s.toSfenString) mustBe s
  }
  "Square#toKifString" must "make KIF-formatted string" in {
    Square.all map (_.toKifString) mustBe kifSquare
  }
  "Square#parseKifString" must "return Some in normal cases" in {
    kifSquare map Square.parseKifString mustBe Square.all
  }
  it must "throw an exception in error cases" in {
    assertThrows[RecordFormatException](Square.parseKifString(""))
    assertThrows[RecordFormatException](Square.parseKifString(" "))
    assertThrows[RecordFormatException](Square.parseKifString("  "))
  }
  it must "recover the original square" in forAll(SquareGen.squares) { s =>
    Square.parseKifString(s.toKifString) mustBe s
  }
  "Square#isPromotionZone" must "return if a piece can promote" in {
    Square(1, 2).isPromotionZone(BLACK) must be(true)
    Square(2, 3).isPromotionZone(BLACK) must be(true)
    Square(3, 4).isPromotionZone(BLACK) must be(false)
    Square(4, 5).isPromotionZone(BLACK) must be(false)
    Square(5, 6).isPromotionZone(BLACK) must be(false)
    Square(6, 7).isPromotionZone(BLACK) must be(false)
    Square(7, 8).isPromotionZone(BLACK) must be(false)
    Square(8, 9).isPromotionZone(BLACK) must be(false)
    Square(9, 1).isPromotionZone(BLACK) must be(true)
    Square(1, 2).isPromotionZone(WHITE) must be(false)
    Square(2, 3).isPromotionZone(WHITE) must be(false)
    Square(3, 4).isPromotionZone(WHITE) must be(false)
    Square(4, 5).isPromotionZone(WHITE) must be(false)
    Square(5, 6).isPromotionZone(WHITE) must be(false)
    Square(6, 7).isPromotionZone(WHITE) must be(true)
    Square(7, 8).isPromotionZone(WHITE) must be(true)
    Square(8, 9).isPromotionZone(WHITE) must be(true)
    Square(9, 1).isPromotionZone(WHITE) must be(false)
  }
  "Square#isLeglZone" must "return if a piece can move there" in {
    Square(1, 2).isLegalZone(Piece(BLACK, PAWN)) must be(true)
    Square(2, 3).isLegalZone(Piece(BLACK, PAWN)) must be(true)
    Square(3, 4).isLegalZone(Piece(BLACK, PAWN)) must be(true)
    Square(4, 5).isLegalZone(Piece(BLACK, PAWN)) must be(true)
    Square(5, 6).isLegalZone(Piece(BLACK, PAWN)) must be(true)
    Square(6, 7).isLegalZone(Piece(BLACK, PAWN)) must be(true)
    Square(7, 8).isLegalZone(Piece(BLACK, PAWN)) must be(true)
    Square(8, 9).isLegalZone(Piece(BLACK, PAWN)) must be(true)
    Square(9, 1).isLegalZone(Piece(BLACK, PAWN)) must be(false)
    Square(8, 1).isLegalZone(Piece(BLACK, LANCE)) must be(false)
    Square(7, 1).isLegalZone(Piece(BLACK, KNIGHT)) must be(false)
    Square(6, 2).isLegalZone(Piece(BLACK, KNIGHT)) must be(false)
    Square(5, 1).isLegalZone(Piece(BLACK, SILVER)) must be(true)
    Square(4, 8).isLegalZone(Piece(WHITE, KNIGHT)) must be(false)
  }

  "Square#getBetweenBB" must "return inner bitboards" in {
    P11.getBetweenBB(P99).toSet must be(Set(P22, P33, P44, P55, P66, P77, P88))
    P99.getBetweenBB(P11).toSet must be(Set(P88, P77, P66, P55, P44, P33, P22))
    P55.getBetweenBB(P77).toSet must be(Set(P66))
    P55.getBetweenBB(P78).toSet must be(Set.empty)
    P55.getBetweenBB(P95).toSet must be(Set(P65, P75, P85))
    P99.getBetweenBB(P99).toSet must be(Set.empty)
    P99.getBetweenBB(P88).toSet must be(Set.empty)
  }

  "Square#getRelation" must "return relationship between squares" in {
    P55.getDisplacement(BLACK, P44) must be(Displacement(DiagonallyForward, 1))
    P55.getDisplacement(BLACK, P54) must be(Displacement(Forward, 1))
    P55.getDisplacement(BLACK, P64) must be(Displacement(DiagonallyForward, 1))
    P55.getDisplacement(BLACK, P45) must be(Displacement(Side, 1))
    P55.getDisplacement(BLACK, P55) must be(Displacement(NoRelation, 0))
    P55.getDisplacement(BLACK, P65) must be(Displacement(Side, 1))
    P55.getDisplacement(BLACK, P46) must be(Displacement(DiagonallyBackward, 1))
    P55.getDisplacement(BLACK, P56) must be(Displacement(Backward, 1))
    P55.getDisplacement(BLACK, P66) must be(Displacement(DiagonallyBackward, 1))
    P55.getDisplacement(BLACK, P43) must be(Displacement(KnightMove, 1))
    P55.getDisplacement(BLACK, P53) must be(Displacement(Forward, 2))
    P55.getDisplacement(BLACK, P63) must be(Displacement(KnightMove, 1))
    P55.getDisplacement(BLACK, P47) must be(Displacement(NoRelation, 0))
    P55.getDisplacement(BLACK, P57) must be(Displacement(Backward, 2))
    P55.getDisplacement(BLACK, P67) must be(Displacement(NoRelation, 0))
    P11.getDisplacement(BLACK, P99) must be(Displacement(DiagonallyBackward, 8))
    P99.getDisplacement(BLACK, P11) must be(Displacement(DiagonallyForward, 8))
    P99.getDisplacement(BLACK, P12) must be(Displacement(NoRelation, 0))
    P32.getDisplacement(BLACK, P92) must be(Displacement(Side, 6))
    P32.getDisplacement(BLACK, P12) must be(Displacement(Side, 2))

    P55.getDisplacement(WHITE, P44) must be(Displacement(DiagonallyBackward, 1))
    P55.getDisplacement(WHITE, P54) must be(Displacement(Backward, 1))
    P55.getDisplacement(WHITE, P64) must be(Displacement(DiagonallyBackward, 1))
    P55.getDisplacement(WHITE, P45) must be(Displacement(Side, 1))
    P55.getDisplacement(WHITE, P55) must be(Displacement(NoRelation, 0))
    P55.getDisplacement(WHITE, P65) must be(Displacement(Side, 1))
    P55.getDisplacement(WHITE, P46) must be(Displacement(DiagonallyForward, 1))
    P55.getDisplacement(WHITE, P56) must be(Displacement(Forward, 1))
    P55.getDisplacement(WHITE, P66) must be(Displacement(DiagonallyForward, 1))
    P55.getDisplacement(WHITE, P43) must be(Displacement(NoRelation, 0))
    P55.getDisplacement(WHITE, P53) must be(Displacement(Backward, 2))
    P55.getDisplacement(WHITE, P63) must be(Displacement(NoRelation, 0))
    P55.getDisplacement(WHITE, P47) must be(Displacement(KnightMove, 1))
    P55.getDisplacement(WHITE, P57) must be(Displacement(Forward, 2))
    P55.getDisplacement(WHITE, P67) must be(Displacement(KnightMove, 1))
    P11.getDisplacement(WHITE, P99) must be(Displacement(DiagonallyForward, 8))
    P99.getDisplacement(WHITE, P11) must be(Displacement(DiagonallyBackward, 8))
    P99.getDisplacement(WHITE, P12) must be(Displacement(NoRelation, 0))
    P32.getDisplacement(WHITE, P92) must be(Displacement(Side, 6))
    P32.getDisplacement(WHITE, P12) must be(Displacement(Side, 2))
  }
}