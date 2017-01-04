package com.mogproject.mogami.core

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, MustMatchers}
import com.mogproject.mogami.core.Player.{BLACK, WHITE}
import com.mogproject.mogami.core.SquareConstant._
import com.mogproject.mogami.core.PieceConstant._
import com.mogproject.mogami.core.Ptype._

class MoveSpec extends FlatSpec with MustMatchers with GeneratorDrivenPropertyChecks {
  val movesForTestCsa = Seq(
    MoveBuilderCsaBoard(BLACK, P77, P76, PAWN, None),
    MoveBuilderCsaBoard(WHITE, P99, P11, PBISHOP, None),
    MoveBuilderCsaHand(BLACK, P82, LANCE, None),
    MoveBuilderCsaBoard(WHITE, P99, P11, PBISHOP, Some(0)),
    MoveBuilderCsaHand(BLACK, P82, Ptype.LANCE, Some(1499))
  )
  val movesForTestSfen = Seq(
    MoveBuilderSfenBoard(P77, P76, promote = false),
    MoveBuilderSfenBoard(P99, P11, promote = true),
    MoveBuilderSfenHand(Ptype.LANCE, P82)
  )
  val csaForTest = Seq("+7776FU", "-9911UM", "+0082KY", "-9911UM,T0", "+0082KY,T1499")
  val sfenForTest = Seq("7g7f", "9i1a+", "L*8b")

  object TestMoveBuilder extends MoveBuilder {
    override def isCheckMove(state: State, from: Option[Square], to: Square, newPtype: Ptype): Boolean = super.isCheckMove(state, from, to, newPtype)

    override def toMove(state: State) = ???
  }

  "MoveBuilder#isCheckMove" must "return true is the move is check" in {
    TestMoveBuilder.isCheckMove(State.HIRATE, Some(P77), P76, PAWN) mustBe false
    TestMoveBuilder.isCheckMove(State.empty.updateHandPiece(BP, 1).get, None, P76, PAWN) mustBe false
    TestMoveBuilder.isCheckMove(State.empty.updateBoardPiece(P51, WK).get.updateHandPiece(BP, 1).get, None, P52, PAWN) mustBe true
  }

  "Move#apply" must "throw an error when the requirements do not meet" in {
    assertThrows[IllegalArgumentException](Move(BLACK, None, P55, PAWN, true, None, false, None))
    assertThrows[IllegalArgumentException](Move(BLACK, None, P55, PAWN, false, Some(PAWN), false, None))
    assertThrows[IllegalArgumentException](Move(BLACK, Some(P56), P55, PAWN, true, None, false, None))
    assertThrows[IllegalArgumentException](Move(BLACK, Some(P56), P56, PAWN, false, None, false, None))
    assertThrows[IllegalArgumentException](Move(BLACK, Some(P56), P54, PAWN, false, None, false, None))
    assertThrows[IllegalArgumentException](Move(BLACK, Some(P11), P98, BISHOP, false, None, false, None))
    assertThrows[IllegalArgumentException](Move(BLACK, Some(P12), P11, PAWN, false, None, false, None))
    assertThrows[IllegalArgumentException](Move(BLACK, Some(P56), P55, PAWN, false, None, false, Some(-123)))
    assertThrows[IllegalArgumentException](Move(BLACK, Some(P56), P55, PAWN, false, Some(KING), false, None))
  }

  "Move#parseCsaString" must "succeed in normal cases" in {
    csaForTest map { c => MoveBuilderCsa.parseCsaString(c) } must be(movesForTestCsa map (Some(_)))
  }
  it must "return None in error cases" in {
    MoveBuilderCsa.parseCsaString("") must be(None)
    MoveBuilderCsa.parseCsaString(" ") must be(None)
    MoveBuilderCsa.parseCsaString("x" * 1000) must be(None)
    MoveBuilderCsa.parseCsaString("=7776FU") must be(None)
    MoveBuilderCsa.parseCsaString("+0176FU") must be(None)
    MoveBuilderCsa.parseCsaString("+7770FU") must be(None)
    MoveBuilderCsa.parseCsaString("+7776FO") must be(None)
    MoveBuilderCsa.parseCsaString("+7776FUU") must be(None)
    MoveBuilderCsa.parseCsaString("+0000FU") must be(None)
    MoveBuilderCsa.parseCsaString("+7777FU") must be(None)
    MoveBuilderCsa.parseCsaString("+7700FU") must be(None)

    MoveBuilderCsa.parseCsaString("+7776FU,") must be(None)
    MoveBuilderCsa.parseCsaString("+7776FU,T") must be(None)
    MoveBuilderCsa.parseCsaString("+7776FU,t1") must be(None)
    MoveBuilderCsa.parseCsaString("+7776FU,Ta") must be(None)
    MoveBuilderCsa.parseCsaString("+7776FU,T10000000000") must be(None)
    MoveBuilderCsa.parseCsaString("+7776FU,T100000000000000000000") must be(None)
    MoveBuilderCsa.parseCsaString("+7776FU,T-1") must be(None)
    MoveBuilderCsa.parseCsaString("+7776FU,,") must be(None)
    MoveBuilderCsa.parseCsaString("+7776FU,T01,") must be(None)
  }
  it must "restore moves" in forAll(MoveGen.movesCsaFormat) { m =>
    MoveBuilderCsa.parseCsaString(m.toCsaString) must be(Some(m))
  }

  "Move#parseSfenString" must "succeed in normal cases" in {
    sfenForTest map { c => MoveBuilderSfen.parseSfenString(c) } must be(movesForTestSfen map (Some(_)))
  }
  it must "return None in error cases" in {
    MoveBuilderSfen.parseSfenString("") must be(None)
    MoveBuilderSfen.parseSfenString(" ") must be(None)
    MoveBuilderSfen.parseSfenString("x" * 1000) must be(None)
    MoveBuilderSfen.parseSfenString("7g76") must be(None)
    MoveBuilderSfen.parseSfenString("7g7F") must be(None)
    MoveBuilderSfen.parseSfenString("L*8b+") must be(None)
    MoveBuilderSfen.parseSfenString("9i1a-") must be(None)
    MoveBuilderSfen.parseSfenString("K*1a") must be(None)
  }
  it must "restore moves" in forAll(MoveGen.movesSfenFormat) { m =>
    MoveBuilderSfen.parseSfenString(m.toSfenString) must be(Some(m))
  }

  "Move#toCsaString" must "describes the move" in {
    movesForTestCsa map (_.toCsaString) must be(csaForTest)
  }

  "Move#toSfenString" must "describes the move" in {
    movesForTestSfen map (_.toSfenString) must be(sfenForTest)
  }

  "MoveBuilderSfen#toMove" must "return move" in {
    val s1: State = State.parseCsaString(Seq(
      "P1 *  *  *  *  *  *  *  * -OU",
      "P2 *  *  *  *  *  *  *  *  * ",
      "P3 *  *  *  *  *  *  *  *  * ",
      "P4 *  *  *  *  *  *  *  *  * ",
      "P5 *  *  *  *  *  *  *  *  * ",
      "P6 *  *  *  *  *  *  *  *  * ",
      "P7 *  * +FU *  *  *  *  *  * ",
      "P8 *  *  *  *  *  *  *  *  * ",
      "P9+KA *  *  *  *  *  *  *  * ",
      "P+00FU",
      "P-",
      "+")).get

    MoveBuilderSfen.parseSfenString("7g7f").get.toMove(s1) mustBe Some(Move(BLACK, Some(P77), P76, PAWN, false, None, true, None))
  }

  "MoveBuilderSfen#apply" must "create instances" in {
    MoveBuilderSfen(Left(P55), P33, false) mustBe MoveBuilderSfenBoard(P55, P33, false)
    MoveBuilderSfen(Left(P55), P33, true) mustBe MoveBuilderSfenBoard(P55, P33, true)
    MoveBuilderSfen(Right(Hand(BP)), P33, false) mustBe MoveBuilderSfenHand(PAWN, P33)
  }
  it must "throw an error when promote is true and from is in hand" in {
    assertThrows[IllegalArgumentException](MoveBuilderSfen(Right(Hand(BP)), P33, true))
  }
}
