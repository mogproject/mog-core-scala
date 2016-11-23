package com.mogproject.mogami.core

import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import com.mogproject.mogami.core.Ptype._

class PtypeSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  val allPtypes = Seq(
    KING, GOLD, PAWN, KNIGHT, SILVER, LANCE, BISHOP, ROOK, PPAWN, PKNIGHT, PSILVER, PLANCE, PBISHOP, PROOK)
  val csaPtypes = Seq("OU", "KI", "FU", "KE", "GI", "KY", "KA", "HI", "TO", "NK", "NG", "NY", "UM", "RY")
  val promotedPtypes = Seq(
    KING, GOLD, PPAWN, PKNIGHT, PSILVER, PLANCE, PBISHOP, PROOK, PPAWN, PKNIGHT, PSILVER, PLANCE, PBISHOP, PROOK)
  val demotedPtypes = Seq(
    KING, GOLD, PAWN, KNIGHT, SILVER, LANCE, BISHOP, ROOK, PAWN, KNIGHT, SILVER, LANCE, BISHOP, ROOK)
  val canPromote = Seq(false, false, true, true, true, true, true, true, false, false, false, false, false, false)
  val isPromoted = Seq(false, false, false, false, false, false, false, false, true, true, true, true, true, true)

  "Ptype#equals" should "distinguish some piece types" in {
    KING.equals(KING) should be(true)
    KING.equals(GOLD) should be(false)
    GOLD.equals(KING) should be(false)
    GOLD.equals(GOLD) should be(true)
  }

  "Ptype#toString" should "describe all piece types" in {
    allPtypes.map(_.toString) should be(Seq(
      "KING", "GOLD", "PAWN", "KNIGHT", "SILVER", "LANCE", "BISHOP", "ROOK",
      "PPAWN", "PKNIGHT", "PSILVER", "PLANCE", "PBISHOP", "PROOK"))
  }

  "Ptype#toCsaString" should "describe all piece types" in {
    allPtypes.map(_.toCsaString) should be(csaPtypes)
  }

  "Ptype#parseCsaString" should "make piece type" in {
    csaPtypes.map(Ptype.parseCsaString) should be(allPtypes.map(Some(_)))
  }
  it should "return None" in {
    Ptype.parseCsaString("") should be(None)
    Ptype.parseCsaString("* ") should be(None)
    Ptype.parseCsaString("OU ") should be(None)
    Ptype.parseCsaString("x" * 100) should be(None)
  }

  it should "recover piece types" in forAll(PtypeGen.ptypes) { pt =>
    Ptype.parseCsaString(pt.toCsaString) should be(Some(pt))
  }

  "Ptype#promoted" should "return promoted piece types" in {
    allPtypes.map(_.promoted) should be(promotedPtypes)
  }

  "Ptype#demoted" should "return demoted piece types" in {
    allPtypes.map(_.demoted) should be(demotedPtypes)
  }

  it should "cancel promotion and demotion" in forAll(PtypeGen.ptypes) { pt =>
    pt.promoted.demoted should be(pt.demoted)
    pt.demoted.promoted should be(pt.promoted)
  }

  "Ptype#canPromote" should "return correctly" in {
    allPtypes.map(_.canPromote) should be(canPromote)
  }

  "Ptype#isBasic" should "return correctly" in {
    allPtypes.map(_.isBasic) should be(isPromoted.map(!_))
  }

  "Ptype#isPromoted" should "return correctly" in {
    allPtypes.map(_.isPromoted) should be(isPromoted)
  }

}
