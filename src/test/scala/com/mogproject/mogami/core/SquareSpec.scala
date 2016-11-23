package com.mogproject.mogami.core

import com.mogproject.mogami.core.Player.{BLACK, WHITE}
import com.mogproject.mogami.core.Ptype.{PAWN, LANCE, KNIGHT, SILVER}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, MustMatchers}

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
}