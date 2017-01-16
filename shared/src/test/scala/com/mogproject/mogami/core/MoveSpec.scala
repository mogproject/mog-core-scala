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
    assertThrows[IllegalArgumentException](Move(BLACK, None, P55, PAWN, true, false, None, None, false, None))
    assertThrows[IllegalArgumentException](Move(BLACK, None, P55, PAWN, false, false, None, Some(PAWN), false, None))
    assertThrows[IllegalArgumentException](Move(BLACK, Some(P56), P55, PAWN, true, false, None, None, false, None))
    assertThrows[IllegalArgumentException](Move(BLACK, Some(P56), P56, PAWN, false, false, None, None, false, None))
    assertThrows[IllegalArgumentException](Move(BLACK, Some(P56), P54, PAWN, false, false, None, None, false, None))
    assertThrows[IllegalArgumentException](Move(BLACK, Some(P11), P98, BISHOP, false, false, None, None, false, None))
    assertThrows[IllegalArgumentException](Move(BLACK, Some(P12), P11, PAWN, false, false, None, None, false, None))
    assertThrows[IllegalArgumentException](Move(BLACK, Some(P56), P55, PAWN, false, false, None, None, false, Some(-123)))
    assertThrows[IllegalArgumentException](Move(BLACK, Some(P56), P55, PAWN, false, false, None, Some(KING), false, None))
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
  "Move#toKifString" must "describes the move" in {
    val states = Seq(
      Seq(
        "P1 *  *  *  *  *  *  *  *  * ",
        "P2 *  * +KI *  *  *  *  *  * ",
        "P3+KI *  *  *  *  *  *  *  * ",
        "P4 *  *  *  *  *  *  *  *  * ",
        "P5 *  *  *  *  *  *  *  *  * ",
        "P6 *  *  *  *  *  *  *  *  * ",
        "P7 *  *  *  *  *  *  *  *  * ",
        "P8 *  *  *  *  *  *  *  *  * ",
        "P9 *  *  *  *  *  *  *  *  * ",
        "P+",
        "P-",
        "+"
      ),
      Seq(
        "P1 *  *  *  *  *  * +KI *  * ",
        "P2 *  *  *  *  *  *  *  *  * ",
        "P3 *  *  *  *  * +KI *  *  * ",
        "P4 *  *  *  *  *  *  *  *  * ",
        "P5 *  *  *  *  *  *  *  *  * ",
        "P6 *  *  *  *  *  *  *  *  * ",
        "P7 *  *  *  *  *  *  *  *  * ",
        "P8 *  *  *  *  *  *  *  *  * ",
        "P9 *  *  *  *  *  *  *  *  * ",
        "P+",
        "P-",
        "+"
      ),
      Seq(
        "P1 *  *  *  *  *  *  *  *  * ",
        "P2 *  *  *  *  *  *  *  *  * ",
        "P3 *  *  *  *  *  *  *  *  * ",
        "P4 *  *  *  *  *  *  *  *  * ",
        "P5 *  *  *  *  * +KI *  *  * ",
        "P6 *  *  *  * +KI *  *  *  * ",
        "P7 *  *  *  *  *  *  *  *  * ",
        "P8 *  *  *  *  *  *  *  *  * ",
        "P9 *  *  *  *  *  *  *  *  * ",
        "P+",
        "P-",
        "+"
      ),
      Seq(
        "P1 *  *  *  *  *  *  *  *  * ",
        "P2 *  *  *  *  *  *  *  *  * ",
        "P3 *  *  *  *  *  *  *  *  * ",
        "P4 *  *  *  *  *  *  *  *  * ",
        "P5 *  *  *  *  *  *  *  *  * ",
        "P6 *  *  *  *  *  *  *  *  * ",
        "P7 *  * +GI *  *  *  *  *  * ",
        "P8 *  *  *  *  *  *  *  *  * ",
        "P9 * +GI *  *  *  *  *  *  * ",
        "P+",
        "P-",
        "+"
      ),
      Seq(
        "P1 *  *  *  *  *  *  *  *  * ",
        "P2 *  *  *  *  *  *  *  *  * ",
        "P3 *  *  *  *  *  *  *  *  * ",
        "P4 *  *  *  *  *  *  *  *  * ",
        "P5 *  *  *  *  *  *  *  *  * ",
        "P6 *  *  *  *  *  *  *  *  * ",
        "P7 *  *  *  *  *  *  * +GI * ",
        "P8 *  *  *  *  *  *  *  *  * ",
        "P9 *  *  *  *  * +GI *  *  * ",
        "P+",
        "P-",
        "+"
      ),
      Seq(
        "P1 *  *  *  *  *  *  *  *  * ",
        "P2+KI * +KI *  *  *  *  *  * ",
        "P3 *  *  *  *  *  *  *  *  * ",
        "P4 *  *  *  *  *  *  *  *  * ",
        "P5 *  *  *  *  *  *  *  *  * ",
        "P6 *  *  *  *  *  *  *  *  * ",
        "P7 *  *  *  *  *  *  *  *  * ",
        "P8 *  *  *  *  *  *  *  *  * ",
        "P9 *  *  *  *  *  *  *  *  * ",
        "P+",
        "P-",
        "+"
      ),
      Seq(
        "P1 *  *  *  *  *  *  *  *  * ",
        "P2 *  *  *  *  *  * +KI * +KI",
        "P3 *  *  *  *  *  *  *  *  * ",
        "P4 *  *  *  *  *  *  *  *  * ",
        "P5 *  *  *  *  *  *  *  *  * ",
        "P6 *  *  *  *  *  *  *  *  * ",
        "P7 *  *  *  *  *  *  *  *  * ",
        "P8 *  *  *  *  *  *  *  *  * ",
        "P9 *  *  *  *  *  *  *  *  * ",
        "P+",
        "P-",
        "+"
      ),
      Seq(
        "P1 *  *  *  *  *  *  *  *  * ",
        "P2 *  *  *  *  *  *  *  *  * ",
        "P3 *  *  *  *  *  *  *  *  * ",
        "P4 *  *  *  *  *  *  *  *  * ",
        "P5 *  *  * +GI * +GI *  *  * ",
        "P6 *  *  *  *  *  *  *  *  * ",
        "P7 *  *  *  *  *  *  *  *  * ",
        "P8 *  *  *  *  *  *  *  *  * ",
        "P9 *  *  *  *  *  *  *  *  * ",
        "P+",
        "P-",
        "+"
      ),
      Seq(
        "P1 *  *  *  *  *  *  *  *  * ",
        "P2 *  *  *  *  *  *  *  *  * ",
        "P3 *  *  *  *  *  *  *  *  * ",
        "P4 *  *  *  *  *  *  *  *  * ",
        "P5 *  *  *  *  *  *  *  *  * ",
        "P6 *  *  *  *  *  *  *  *  * ",
        "P7 *  *  *  *  *  *  *  *  * ",
        "P8 *  *  *  *  *  *  *  *  * ",
        "P9 * +KI+KI *  *  *  *  *  * ",
        "P+",
        "P-",
        "+"
      ),
      Seq(
        "P1 *  *  *  *  *  *  *  *  * ",
        "P2 *  *  *  *  *  *  *  *  * ",
        "P3 *  *  *  *  *  *  *  *  * ",
        "P4 *  *  *  *  *  *  *  *  * ",
        "P5 *  *  *  *  *  *  *  *  * ",
        "P6 *  *  *  *  *  *  *  *  * ",
        "P7 *  *  *  *  *  *  *  *  * ",
        "P8 *  *  *  *  *  *  *  *  * ",
        "P9 *  *  *  *  *  * +GI+GI * ",
        "P+",
        "P-",
        "+"
      ),
      Seq(
        "P1 *  *  *  *  *  *  *  *  * ",
        "P2 *  *  *  *  *  *  *  *  * ",
        "P3 *  *  * +KI+KI+KI *  *  * ",
        "P4 *  *  *  *  *  *  *  *  * ",
        "P5 *  *  *  *  *  *  *  *  * ",
        "P6 *  *  *  *  *  *  *  *  * ",
        "P7 *  *  *  *  *  *  *  *  * ",
        "P8 *  *  *  *  *  *  *  *  * ",
        "P9 *  *  *  *  *  *  *  *  * ",
        "P+",
        "P-",
        "+"
      ),
      Seq(
        "P1 *  *  *  *  *  *  *  *  * ",
        "P2 *  *  *  *  *  *  *  *  * ",
        "P3 *  *  *  *  *  *  *  *  * ",
        "P4 *  *  *  *  *  *  *  *  * ",
        "P5 *  *  *  *  *  *  *  *  * ",
        "P6 *  *  *  *  *  *  *  *  * ",
        "P7 * +TO *  *  *  *  *  *  * ",
        "P8+TO *  *  *  *  *  *  *  * ",
        "P9+TO+TO+TO *  *  *  *  *  * ",
        "P+",
        "P-",
        "+"
      ),
      Seq(
        "P1 *  *  *  *  *  *  *  *  * ",
        "P2 *  *  *  *  *  *  *  *  * ",
        "P3 *  *  *  *  *  *  *  *  * ",
        "P4 *  *  *  *  *  *  *  *  * ",
        "P5 *  *  *  *  *  *  *  *  * ",
        "P6 *  *  *  *  *  *  *  *  * ",
        "P7 *  *  *  *  *  * +GI * +GI",
        "P8 *  *  *  *  *  *  *  *  * ",
        "P9 *  *  *  *  *  * +GI+GI * ",
        "P+",
        "P-",
        "+"
      ),
      Seq(
        "P1+RY *  *  *  *  *  *  *  * ",
        "P2 *  *  *  *  *  *  *  *  * ",
        "P3 *  *  *  *  *  *  *  *  * ",
        "P4 * +RY *  *  *  *  *  *  * ",
        "P5 *  *  *  *  *  *  *  *  * ",
        "P6 *  *  *  *  *  *  *  *  * ",
        "P7 *  *  *  *  *  *  *  *  * ",
        "P8 *  *  *  *  *  *  *  *  * ",
        "P9 *  *  *  *  *  *  *  *  * ",
        "P+",
        "P-",
        "+"
      ),
      Seq(
        "P1 *  *  *  *  *  *  *  *  * ",
        "P2 *  *  *  * +RY *  *  *  * ",
        "P3 *  *  *  *  *  *  * +RY * ",
        "P4 *  *  *  *  *  *  *  *  * ",
        "P5 *  *  *  *  *  *  *  *  * ",
        "P6 *  *  *  *  *  *  *  *  * ",
        "P7 *  *  *  *  *  *  *  *  * ",
        "P8 *  *  *  *  *  *  *  *  * ",
        "P9 *  *  *  *  *  *  *  *  * ",
        "P+",
        "P-",
        "+"
      ),
      Seq(
        "P1 *  *  *  *  *  *  *  *  * ",
        "P2 *  *  *  *  *  *  *  *  * ",
        "P3 *  *  *  *  *  *  *  *  * ",
        "P4 *  *  *  *  *  *  *  *  * ",
        "P5 *  *  *  * +RY *  *  * +RY",
        "P6 *  *  *  *  *  *  *  *  * ",
        "P7 *  *  *  *  *  *  *  *  * ",
        "P8 *  *  *  *  *  *  *  *  * ",
        "P9 *  *  *  *  *  *  *  *  * ",
        "P+",
        "P-",
        "+"
      ),
      Seq(
        "P1 *  *  *  *  *  *  *  *  * ",
        "P2 *  *  *  *  *  *  *  *  * ",
        "P3 *  *  *  *  *  *  *  *  * ",
        "P4 *  *  *  *  *  *  *  *  * ",
        "P5 *  *  *  *  *  *  *  *  * ",
        "P6 *  *  *  *  *  *  *  *  * ",
        "P7 *  *  *  *  *  *  *  *  * ",
        "P8 *  *  *  *  *  *  *  *  * ",
        "P9+RY+RY *  *  *  *  *  *  * ",
        "P+",
        "P-",
        "+"
      ),
      Seq(
        "P1 *  *  *  *  *  *  *  *  * ",
        "P2 *  *  *  *  *  *  *  *  * ",
        "P3 *  *  *  *  *  *  *  *  * ",
        "P4 *  *  *  *  *  *  *  *  * ",
        "P5 *  *  *  *  *  *  *  *  * ",
        "P6 *  *  *  *  *  *  *  *  * ",
        "P7 *  *  *  *  *  *  *  *  * ",
        "P8 *  *  *  *  *  *  * +RY * ",
        "P9 *  *  *  *  *  *  *  * +RY",
        "P+",
        "P-",
        "+"
      ),
      Seq(
        "P1+UM+UM *  *  *  *  *  *  * ",
        "P2 *  *  *  *  *  *  *  *  * ",
        "P3 *  *  *  *  *  *  *  *  * ",
        "P4 *  *  *  *  *  *  *  *  * ",
        "P5 *  *  *  *  *  *  *  *  * ",
        "P6 *  *  *  *  *  *  *  *  * ",
        "P7 *  *  *  *  *  *  *  *  * ",
        "P8 *  *  *  *  *  *  *  *  * ",
        "P9 *  *  *  *  *  *  *  *  * ",
        "P+",
        "P-",
        "+"
      ),
      Seq(
        "P1 *  *  *  *  *  *  *  *  * ",
        "P2 *  *  *  *  *  *  *  *  * ",
        "P3 *  *  * +UM *  *  *  *  * ",
        "P4 *  *  *  *  *  *  *  *  * ",
        "P5+UM *  *  *  *  *  *  *  * ",
        "P6 *  *  *  *  *  *  *  *  * ",
        "P7 *  *  *  *  *  *  *  *  * ",
        "P8 *  *  *  *  *  *  *  *  * ",
        "P9 *  *  *  *  *  *  *  *  * ",
        "P+",
        "P-",
        "+"
      ),
      Seq(
        "P1 *  *  *  *  *  *  *  * +UM",
        "P2 *  *  *  *  *  *  *  *  * ",
        "P3 *  *  *  *  *  *  *  *  * ",
        "P4 *  *  *  *  *  * +UM *  * ",
        "P5 *  *  *  *  *  *  *  *  * ",
        "P6 *  *  *  *  *  *  *  *  * ",
        "P7 *  *  *  *  *  *  *  *  * ",
        "P8 *  *  *  *  *  *  *  *  * ",
        "P9 *  *  *  *  *  *  *  *  * ",
        "P+",
        "P-",
        "+"
      ),
      Seq(
        "P1 *  *  *  *  *  *  *  *  * ",
        "P2 *  *  *  *  *  *  *  *  * ",
        "P3 *  *  *  *  *  *  *  *  * ",
        "P4 *  *  *  *  *  *  *  *  * ",
        "P5 *  *  *  *  *  *  *  *  * ",
        "P6 *  *  *  *  *  *  *  *  * ",
        "P7 *  *  *  *  *  *  *  *  * ",
        "P8 *  *  *  *  *  *  *  *  * ",
        "P9+UM *  *  * +UM *  *  *  * ",
        "P+",
        "P-",
        "+"
      ),
      Seq(
        "P1 *  *  *  *  *  *  *  *  * ",
        "P2 *  *  *  *  *  *  *  *  * ",
        "P3 *  *  *  *  *  *  *  *  * ",
        "P4 *  *  *  *  *  *  *  *  * ",
        "P5 *  *  *  *  *  *  *  *  * ",
        "P6 *  *  *  *  *  *  *  *  * ",
        "P7 *  *  *  *  * +UM *  *  * ",
        "P8 *  *  *  *  *  *  *  * +UM",
        "P9 *  *  *  *  *  *  *  *  * ",
        "P+",
        "P-",
        "+"
      ),
      Seq(
        "P1 *  *  * -KY *  *  *  *  * ",
        "P2 *  *  *  *  *  *  *  *  * ",
        "P3 *  *  *  *  *  *  *  *  * ",
        "P4 * -KA *  *  *  *  *  *  * ",
        "P5 *  *  *  *  *  *  *  *  * ",
        "P6 *  *  *  *  *  *  *  *  * ",
        "P7 *  *  *  * -KA *  *  *  * ",
        "P8 *  *  *  *  *  *  *  *  * ",
        "P9 *  *  *  *  *  *  *  *  * ",
        "P+",
        "P-00KY",
        "-"
      )
    ).map(State.parseCsaString(_).get)

    MoveBuilderSfen.parseSfenString("7g7f").get.toMove(State.HIRATE).get.toKifString mustBe "７六歩"

    MoveBuilderCsa.parseCsaString("+9382KI").get.toMove(states(0)).get.toKifString mustBe "８二金上"
    MoveBuilderCsa.parseCsaString("+7282KI").get.toMove(states(0)).get.toKifString mustBe "８二金寄"
    MoveBuilderCsa.parseCsaString("+4332KI").get.toMove(states(1)).get.toKifString mustBe "３二金上"
    MoveBuilderCsa.parseCsaString("+3132KI").get.toMove(states(1)).get.toKifString mustBe "３二金引"
    MoveBuilderCsa.parseCsaString("+5655KI").get.toMove(states(2)).get.toKifString mustBe "５五金上"
    MoveBuilderCsa.parseCsaString("+4555KI").get.toMove(states(2)).get.toKifString mustBe "５五金寄"
    MoveBuilderCsa.parseCsaString("+8988GI").get.toMove(states(3)).get.toKifString mustBe "８八銀上"
    MoveBuilderCsa.parseCsaString("+7788GI").get.toMove(states(3)).get.toKifString mustBe "８八銀引"
    MoveBuilderCsa.parseCsaString("+4938GI").get.toMove(states(4)).get.toKifString mustBe "３八銀上"
    MoveBuilderCsa.parseCsaString("+2738GI").get.toMove(states(4)).get.toKifString mustBe "３八銀引"

    MoveBuilderCsa.parseCsaString("+9281KI").get.toMove(states(5)).get.toKifString mustBe "８一金左"
    MoveBuilderCsa.parseCsaString("+7281KI").get.toMove(states(5)).get.toKifString mustBe "８一金右"
    MoveBuilderCsa.parseCsaString("+3222KI").get.toMove(states(6)).get.toKifString mustBe "２二金左"
    MoveBuilderCsa.parseCsaString("+1222KI").get.toMove(states(6)).get.toKifString mustBe "２二金右"
    MoveBuilderCsa.parseCsaString("+6556GI").get.toMove(states(7)).get.toKifString mustBe "５六銀左"
    MoveBuilderCsa.parseCsaString("+4556GI").get.toMove(states(7)).get.toKifString mustBe "５六銀右"
    MoveBuilderCsa.parseCsaString("+8978KI").get.toMove(states(8)).get.toKifString mustBe "７八金左"
    MoveBuilderCsa.parseCsaString("+7978KI").get.toMove(states(8)).get.toKifString mustBe "７八金直"
    MoveBuilderCsa.parseCsaString("+3938GI").get.toMove(states(9)).get.toKifString mustBe "３八銀直"
    MoveBuilderCsa.parseCsaString("+2938GI").get.toMove(states(9)).get.toKifString mustBe "３八銀右"

    MoveBuilderCsa.parseCsaString("+6352KI").get.toMove(states(10)).get.toKifString mustBe "５二金左"
    MoveBuilderCsa.parseCsaString("+5352KI").get.toMove(states(10)).get.toKifString mustBe "５二金直"
    MoveBuilderCsa.parseCsaString("+4352KI").get.toMove(states(10)).get.toKifString mustBe "５二金右"
    MoveBuilderCsa.parseCsaString("+7988TO").get.toMove(states(11)).get.toKifString mustBe "８八と右"
    MoveBuilderCsa.parseCsaString("+8988TO").get.toMove(states(11)).get.toKifString mustBe "８八と直"
    MoveBuilderCsa.parseCsaString("+9988TO").get.toMove(states(11)).get.toKifString mustBe "８八と左上"
    MoveBuilderCsa.parseCsaString("+9888TO").get.toMove(states(11)).get.toKifString mustBe "８八と寄"
    MoveBuilderCsa.parseCsaString("+8788TO").get.toMove(states(11)).get.toKifString mustBe "８八と引"
    MoveBuilderCsa.parseCsaString("+2928GI").get.toMove(states(12)).get.toKifString mustBe "２八銀直"
    MoveBuilderCsa.parseCsaString("+1728GI").get.toMove(states(12)).get.toKifString mustBe "２八銀右"
    MoveBuilderCsa.parseCsaString("+3928GI").get.toMove(states(12)).get.toKifString mustBe "２八銀左上"
    MoveBuilderCsa.parseCsaString("+3728GI").get.toMove(states(12)).get.toKifString mustBe "２八銀左引"

    MoveBuilderCsa.parseCsaString("+9182RY").get.toMove(states(13)).get.toKifString mustBe "８二竜引"
    MoveBuilderCsa.parseCsaString("+8482RY").get.toMove(states(13)).get.toKifString mustBe "８二竜上"
    MoveBuilderCsa.parseCsaString("+2343RY").get.toMove(states(14)).get.toKifString mustBe "４三竜寄"
    MoveBuilderCsa.parseCsaString("+5243RY").get.toMove(states(14)).get.toKifString mustBe "４三竜引"
    MoveBuilderCsa.parseCsaString("+5535RY").get.toMove(states(15)).get.toKifString mustBe "３五竜左"
    MoveBuilderCsa.parseCsaString("+1535RY").get.toMove(states(15)).get.toKifString mustBe "３五竜右"
    MoveBuilderCsa.parseCsaString("+9988RY").get.toMove(states(16)).get.toKifString mustBe "８八竜左"
    MoveBuilderCsa.parseCsaString("+8988RY").get.toMove(states(16)).get.toKifString mustBe "８八竜右"
    MoveBuilderCsa.parseCsaString("+2817RY").get.toMove(states(17)).get.toKifString mustBe "１七竜左"
    MoveBuilderCsa.parseCsaString("+1917RY").get.toMove(states(17)).get.toKifString mustBe "１七竜右"

    MoveBuilderCsa.parseCsaString("+9182UM").get.toMove(states(18)).get.toKifString mustBe "８二馬左"
    MoveBuilderCsa.parseCsaString("+8182UM").get.toMove(states(18)).get.toKifString mustBe "８二馬右"
    MoveBuilderCsa.parseCsaString("+9585UM").get.toMove(states(19)).get.toKifString mustBe "８五馬寄"
    MoveBuilderCsa.parseCsaString("+6385UM").get.toMove(states(19)).get.toKifString mustBe "８五馬引"
    MoveBuilderCsa.parseCsaString("+1112UM").get.toMove(states(20)).get.toKifString mustBe "１二馬引"
    MoveBuilderCsa.parseCsaString("+3412UM").get.toMove(states(20)).get.toKifString mustBe "１二馬上"
    MoveBuilderCsa.parseCsaString("+9977UM").get.toMove(states(21)).get.toKifString mustBe "７七馬左"
    MoveBuilderCsa.parseCsaString("+5977UM").get.toMove(states(21)).get.toKifString mustBe "７七馬右"
    MoveBuilderCsa.parseCsaString("+4729UM").get.toMove(states(22)).get.toKifString mustBe "２九馬左"
    MoveBuilderCsa.parseCsaString("+1829UM").get.toMove(states(22)).get.toKifString mustBe "２九馬右"

    MoveBuilderCsa.parseCsaString("-5766KA").get.toMove(states(23)).get.toKifString mustBe "６六角引不成"
    MoveBuilderCsa.parseCsaString("-5766UM").get.toMove(states(23)).get.toKifString mustBe "６六角引成"
    MoveBuilderCsa.parseCsaString("-6168KY").get.toMove(states(23)).get.toKifString mustBe "６八香不成"
    MoveBuilderCsa.parseCsaString("-6168NY").get.toMove(states(23)).get.toKifString mustBe "６八香成"
    MoveBuilderCsa.parseCsaString("-0068KY").get.toMove(states(23)).get.toKifString mustBe "６八香打"
  }
  "Move#toWesternNotationString" must "describes the move" in {
    val states = Seq(
      Seq(
        "P1 *  *  *  *  *  *  *  *  * ",
        "P2+GI *  *  *  *  *  *  *  * ",
        "P3 * -KI *  * +UM *  *  *  * ",
        "P4+GI *  *  *  *  *  *  *  * ",
        "P5 *  *  *  * +KE * +KE *  * ",
        "P6 *  *  *  *  *  *  *  *  * ",
        "P7 *  *  *  *  *  *  *  *  * ",
        "P8 *  *  *  *  *  *  *  *  * ",
        "P9 *  *  *  *  *  *  *  *  * ",
        "P+00KY",
        "P-",
        "+"
      )
    ).map(State.parseCsaString(_).get)

    MoveBuilderCsa.parseCsaString("+7776FU").get.toMove(State.HIRATE).get.toWesternNotationString mustBe "P-7f"
    MoveBuilderCsa.parseCsaString("-3334FU").get.toMove(State.HIRATE.copy(turn = WHITE)).get.toWesternNotationString mustBe "P-3d"
    MoveBuilderCsa.parseCsaString("+5352UM").get.toMove(states(0)).get.toWesternNotationString mustBe "+B-5b"
    MoveBuilderCsa.parseCsaString("+0033KY").get.toMove(states(0)).get.toWesternNotationString mustBe "L*3c"
    MoveBuilderCsa.parseCsaString("+5563KE").get.toMove(states(0)).get.toWesternNotationString mustBe "N-6c="
    MoveBuilderCsa.parseCsaString("+5563NK").get.toMove(states(0)).get.toWesternNotationString mustBe "N-6c+"
    MoveBuilderCsa.parseCsaString("+5543KE").get.toMove(states(0)).get.toWesternNotationString mustBe "N5e-4c="
    MoveBuilderCsa.parseCsaString("+5543NK").get.toMove(states(0)).get.toWesternNotationString mustBe "N5e-4c+"
    MoveBuilderCsa.parseCsaString("+9283GI").get.toMove(states(0)).get.toWesternNotationString mustBe "S9bx8c="
    MoveBuilderCsa.parseCsaString("+9283NG").get.toMove(states(0)).get.toWesternNotationString mustBe "S9bx8c+"
    MoveBuilderCsa.parseCsaString("+9483GI").get.toMove(states(0)).get.toWesternNotationString mustBe "S9dx8c="
    MoveBuilderCsa.parseCsaString("+9483NG").get.toMove(states(0)).get.toWesternNotationString mustBe "S9dx8c+"
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

    MoveBuilderSfen.parseSfenString("7g7f").get.toMove(s1) mustBe Some(Move(BLACK, Some(P77), P76, PAWN, false, false, None, None, true, None))
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
