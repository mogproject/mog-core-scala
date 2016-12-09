package com.mogproject.mogami.core

import com.mogproject.mogami.core.Player.{BLACK, WHITE}
import com.mogproject.mogami.core.Ptype.{PAWN, LANCE, KNIGHT, SILVER}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, MustMatchers}
import com.mogproject.mogami.core.SquareConstant._
import com.mogproject.mogami.core.SquareRelation._

class SquareSpec extends FlatSpec with MustMatchers with GeneratorDrivenPropertyChecks {
  private val allSquare = Square.HAND +: (for (f <- 1 to 9; r <- 1 to 9) yield Square(f, r))
  private val csaSquare = "00" +: (for (f <- '1' to '9'; r <- '1' to '9') yield s"$f$r")
  private val sfenSquare = "*" +: (for (f <- '1' to '9'; r <- 'a' to 'i') yield s"$f$r")

  "Square#parseCsaString" must "return Some in normal cases" in {
    csaSquare map { c => Square.parseCsaString(c) } mustBe allSquare.map(Some(_))
  }
  it must "return None in error cases" in {
    Square.parseCsaString("") mustBe None
    Square.parseCsaString(" ") mustBe None
    Square.parseCsaString("x" * 1000) mustBe None
    Square.parseCsaString("01") mustBe None
    Square.parseCsaString("90") mustBe None
    Square.parseCsaString("123") mustBe None
  }
  "Square#toCsaString" must "make CSA-formatted string" in {
    allSquare map (_.toCsaString) mustBe csaSquare
  }
  it must "recover the original square" in forAll(SquareGen.squares) { s =>
    Square.parseCsaString(s.toCsaString) must be(Some(s))
  }
  "Square#parseSfenString" must "return Some in normal cases" in {
    sfenSquare map { c => Square.parseSfenString(c) } mustBe allSquare.map(Some(_))
  }
  it must "return None in error cases" in {
    Square.parseCsaString("") mustBe None
    Square.parseCsaString(" ") mustBe None
    Square.parseCsaString("x" * 1000) mustBe None
    Square.parseCsaString("01") mustBe None
    Square.parseCsaString("90") mustBe None
    Square.parseCsaString("123") mustBe None
  }
  "Square#toSfenString" must "make CSA-formatted string" in {
    allSquare map (_.toSfenString) mustBe sfenSquare
  }
  it must "recover the original square" in forAll(SquareGen.squares) { s =>
    Square.parseSfenString(s.toSfenString) must be(Some(s))
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

  "Square#innerSquares" must "return inner squares" in {
    P11.getInnerSquares(P99) must be(Seq(P22, P33, P44, P55, P66, P77, P88))
    P99.getInnerSquares(P11) must be(Seq(P88, P77, P66, P55, P44, P33, P22))
    P55.getInnerSquares(P77) must be(Seq(P66))
    P55.getInnerSquares(P78) must be(Seq.empty)
    P55.getInnerSquares(P95) must be(Seq(P65, P75, P85))
    P99.getInnerSquares(P99) must be(Seq.empty)
    P99.getInnerSquares(P88) must be(Seq.empty)
  }

  "Square#getRelation" must "return relationship between squares" in {
    P55.getRelation(BLACK, P44) must be((DiagonallyForward, 1))
    P55.getRelation(BLACK, P54) must be((Forward, 1))
    P55.getRelation(BLACK, P64) must be((DiagonallyForward, 1))
    P55.getRelation(BLACK, P45) must be((Side, 1))
    P55.getRelation(BLACK, P55) must be((NoRelation, 0))
    P55.getRelation(BLACK, P65) must be((Side, 1))
    P55.getRelation(BLACK, P46) must be((DiagonallyBackward, 1))
    P55.getRelation(BLACK, P56) must be((Backward, 1))
    P55.getRelation(BLACK, P66) must be((DiagonallyBackward, 1))
    P55.getRelation(BLACK, P43) must be((KnightMove, 1))
    P55.getRelation(BLACK, P53) must be((Forward, 2))
    P55.getRelation(BLACK, P63) must be((KnightMove, 1))
    P55.getRelation(BLACK, P47) must be((NoRelation, 0))
    P55.getRelation(BLACK, P57) must be((Backward, 2))
    P55.getRelation(BLACK, P67) must be((NoRelation, 0))
    P11.getRelation(BLACK, P99) must be((DiagonallyBackward, 8))
    P99.getRelation(BLACK, P11) must be((DiagonallyForward, 8))
    P99.getRelation(BLACK, P12) must be((NoRelation, 0))
    P32.getRelation(BLACK, P92) must be((Side, 6))
    P32.getRelation(BLACK, P12) must be((Side, 2))

    P55.getRelation(WHITE, P44) must be((DiagonallyBackward, 1))
    P55.getRelation(WHITE, P54) must be((Backward, 1))
    P55.getRelation(WHITE, P64) must be((DiagonallyBackward, 1))
    P55.getRelation(WHITE, P45) must be((Side, 1))
    P55.getRelation(WHITE, P55) must be((NoRelation, 0))
    P55.getRelation(WHITE, P65) must be((Side, 1))
    P55.getRelation(WHITE, P46) must be((DiagonallyForward, 1))
    P55.getRelation(WHITE, P56) must be((Forward, 1))
    P55.getRelation(WHITE, P66) must be((DiagonallyForward, 1))
    P55.getRelation(WHITE, P43) must be((NoRelation, 0))
    P55.getRelation(WHITE, P53) must be((Backward, 2))
    P55.getRelation(WHITE, P63) must be((NoRelation, 0))
    P55.getRelation(WHITE, P47) must be((KnightMove, 1))
    P55.getRelation(WHITE, P57) must be((Forward, 2))
    P55.getRelation(WHITE, P67) must be((KnightMove, 1))
    P11.getRelation(WHITE, P99) must be((DiagonallyForward, 8))
    P99.getRelation(WHITE, P11) must be((DiagonallyBackward, 8))
    P99.getRelation(WHITE, P12) must be((NoRelation, 0))
    P32.getRelation(WHITE, P92) must be((Side, 6))
    P32.getRelation(WHITE, P12) must be((Side, 2))
  }
}