package com.mogproject.mogami.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import com.mogproject.mogami._
import com.mogproject.mogami.core.io.RecordFormatException

class PtypeSpec extends AnyFlatSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  val allPtypes = Seq(
    KING, GOLD, PAWN, KNIGHT, SILVER, LANCE, BISHOP, ROOK, PPAWN, PKNIGHT, PSILVER, PLANCE, PBISHOP, PROOK)
  val csaPtypes = Seq(
    "OU", "KI", "FU", "KE", "GI", "KY", "KA", "HI", "TO", "NK", "NG", "NY", "UM", "RY")
  val englishNames = Seq(
    "K", "G", "P", "N", "S", "L", "B", "R", "+P", "+N", "+S", "+L", "+B", "+R")
  val japaneseNames = Seq(
    "玉", "金", "歩", "桂", "銀", "香", "角", "飛", "と", "圭", "全", "杏", "馬", "龍")
  val promotedPtypes = Seq(
    KING, GOLD, PPAWN, PKNIGHT, PSILVER, PLANCE, PBISHOP, PROOK, PPAWN, PKNIGHT, PSILVER, PLANCE, PBISHOP, PROOK)
  val demotedPtypes = Seq(
    KING, GOLD, PAWN, KNIGHT, SILVER, LANCE, BISHOP, ROOK, PAWN, KNIGHT, SILVER, LANCE, BISHOP, ROOK)
  val canPromote = Seq(false, false, true, true, true, true, true, true, false, false, false, false, false, false)
  val isPromoted = Seq(false, false, false, false, false, false, false, false, true, true, true, true, true, true)

  "Ptype#equals" must "distinguish some piece types" in {
    KING.equals(KING) must be(true)
    KING.equals(GOLD) must be(false)
    GOLD.equals(KING) must be(false)
    GOLD.equals(GOLD) must be(true)
  }

  "Ptype#toString" must "describe all piece types" in {
    allPtypes.map(_.toString) must be(Seq(
      "KING", "GOLD", "PAWN", "KNIGHT", "SILVER", "LANCE", "BISHOP", "ROOK",
      "PPAWN", "PKNIGHT", "PSILVER", "PLANCE", "PBISHOP", "PROOK"))
  }

  "Ptype#toCsaString" must "describe all piece types" in {
    allPtypes.map(_.toCsaString) must be(csaPtypes)
  }

  "Ptype#parseCsaString" must "make piece type" in {
    csaPtypes.map(Ptype.parseCsaString) mustBe allPtypes
  }
  it must "throw an exception in error cases" in {
    assertThrows[RecordFormatException](Ptype.parseCsaString(""))
    assertThrows[RecordFormatException](Ptype.parseCsaString("* "))
    assertThrows[RecordFormatException](Ptype.parseCsaString("OU "))
    assertThrows[RecordFormatException](Ptype.parseCsaString("x" * 100))
  }

  "Ptype#toEnglishSimpleName" must "describe all piece types" in {
    allPtypes.map(_.toEnglishSimpleName) must be(englishNames)
  }

  "Ptype#toJapaneseSimpleName" must "describe all piece types" in {
    allPtypes.map(_.toJapaneseSimpleName) must be(japaneseNames)
  }

  it must "recover piece types" in forAll(PtypeGen.ptypes) { pt =>
    Ptype.parseCsaString(pt.toCsaString) mustBe pt
  }

  "Ptype#promoted" must "return promoted piece types" in {
    allPtypes.map(_.promoted) must be(promotedPtypes)
  }

  "Ptype#demoted" must "return demoted piece types" in {
    allPtypes.map(_.demoted) must be(demotedPtypes)
  }
  it must "cancel promotion and demotion" in forAll(PtypeGen.ptypes) { pt =>
    pt.promoted.demoted must be(pt.demoted)
    pt.demoted.promoted must be(pt.promoted)
  }

  "Ptype#canPromote" must "return correctly" in {
    allPtypes.map(_.canPromote) must be(canPromote)
  }

  "Ptype#isBasic" must "return correctly" in {
    allPtypes.map(_.isBasic) must be(isPromoted.map(!_))
  }

  "Ptype#isPromoted" must "return correctly" in {
    allPtypes.map(_.isPromoted) must be(isPromoted)
  }

}
