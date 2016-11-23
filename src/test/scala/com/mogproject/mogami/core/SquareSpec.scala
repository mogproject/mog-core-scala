package com.mogproject.mogami.core

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
    Piece.parseCsaString("") mustBe None
    Piece.parseCsaString(" ") mustBe None
    Piece.parseCsaString("x" * 1000) mustBe None
    Piece.parseCsaString("01") mustBe None
    Piece.parseCsaString("90") mustBe None
    Piece.parseCsaString("123") mustBe None
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
    Piece.parseCsaString("") mustBe None
    Piece.parseCsaString(" ") mustBe None
    Piece.parseCsaString("x" * 1000) mustBe None
    Piece.parseCsaString("01") mustBe None
    Piece.parseCsaString("90") mustBe None
    Piece.parseCsaString("123") mustBe None
  }
  "Square#toSfenString" must "make CSA-formatted string" in {
    allSquare map (_.toSfenString) mustBe sfenSquare
  }
  it must "recover the original square" in forAll(SquareGen.squares) { s =>
    Square.parseSfenString(s.toSfenString) must be(Some(s))
  }
}