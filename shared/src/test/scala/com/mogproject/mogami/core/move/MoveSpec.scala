package com.mogproject.mogami.core.move

import com.mogproject.mogami.core.PieceConstant._
import com.mogproject.mogami.core.Player.{BLACK, WHITE}
import com.mogproject.mogami.core.Ptype._
import com.mogproject.mogami.core.SquareConstant._
import com.mogproject.mogami.core.io.RecordFormatException
import com.mogproject.mogami.core.{Hand, Ptype, Square, State}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, MustMatchers}

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
  val movesForTestKif = Seq(
    MoveBuilderKifBoard(P77, Some(P76), PAWN, promote = false, None),
    MoveBuilderKifBoard(P88, Some(P22), BISHOP, promote = true, None),
    MoveBuilderKifBoard(P31, None, SILVER, promote = false, None),
    MoveBuilderKifBoard(P23, None, LANCE, promote = true, None),
    MoveBuilderKifHand(P55, KNIGHT, None),
    MoveBuilderKifBoard(P55, Some(P43), PKNIGHT, promote = false, None),
    MoveBuilderKifBoard(P44, Some(P43), PSILVER, promote = false, None),
    MoveBuilderKifBoard(P77, Some(P76), PAWN, promote = false, Some(0)),
    MoveBuilderKifBoard(P77, Some(P76), PAWN, promote = false, Some(1)),
    MoveBuilderKifBoard(P77, Some(P76), PAWN, promote = false, Some(60)),
    MoveBuilderKifBoard(P77, Some(P76), PAWN, promote = false, Some(999)),
    MoveBuilderKifBoard(P77, Some(P76), PAWN, promote = false, Some(Int.MaxValue))
  )
  val csaForTest = Seq("+7776FU", "-9911UM", "+0082KY", "-9911UM,T0", "+0082KY,T1499")
  val sfenForTest = Seq("7g7f", "9i1a+", "L*8b")
  val kifForTest = Seq(
    "７六歩(77)", "２二角成(88)", "同　銀(31)", "同　香成(23)", "５五桂打", "４三成桂(55)", "４三成銀(44)",
    "７六歩(77) (00:00/)", "７六歩(77) (00:01/)", "７六歩(77) (01:00/)", "７六歩(77) (16:39/)", "７六歩(77) (35791394:07/)"
  )

  object TestMoveBuilder extends MoveBuilder {
    override def isCheckMove(state: State, from: Option[Square], to: Square, newPtype: Ptype): Boolean = super.isCheckMove(state, from, to, newPtype)

    override def toMove(state: State, lastMoveTo: Option[Square] = None, isStrict: Boolean = true) = ???
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

  "Move#toCsaString" must "describe the move" in {
    movesForTestCsa map (_.toCsaString) must be(csaForTest)
  }
  "Move#parseCsaString" must "succeed in normal cases" in {
    csaForTest map { c => MoveBuilderCsa.parseCsaString(c) } mustBe movesForTestCsa
  }
  it must "throw an exception in error cases" in {
    assertThrows[RecordFormatException](MoveBuilderCsa.parseCsaString(""))
    assertThrows[RecordFormatException](MoveBuilderCsa.parseCsaString(" "))
    assertThrows[RecordFormatException](MoveBuilderCsa.parseCsaString("x" * 1000))
    assertThrows[RecordFormatException](MoveBuilderCsa.parseCsaString("=7776FU"))
    assertThrows[RecordFormatException](MoveBuilderCsa.parseCsaString("+0176FU"))
    assertThrows[RecordFormatException](MoveBuilderCsa.parseCsaString("+7770FU"))
    assertThrows[RecordFormatException](MoveBuilderCsa.parseCsaString("+7776FO"))
    assertThrows[RecordFormatException](MoveBuilderCsa.parseCsaString("+7776FUU"))
    assertThrows[RecordFormatException](MoveBuilderCsa.parseCsaString("+0000FU"))
    assertThrows[RecordFormatException](MoveBuilderCsa.parseCsaString("+7777FU"))
    assertThrows[RecordFormatException](MoveBuilderCsa.parseCsaString("+7700FU"))

    assertThrows[RecordFormatException](MoveBuilderCsa.parseCsaString("+7776FU,T"))
    assertThrows[RecordFormatException](MoveBuilderCsa.parseCsaString("+7776FU,t1"))
    assertThrows[RecordFormatException](MoveBuilderCsa.parseCsaString("+7776FU,Ta"))
    assertThrows[RecordFormatException](MoveBuilderCsa.parseCsaString("+7776FU,T10000000000"))
    assertThrows[RecordFormatException](MoveBuilderCsa.parseCsaString("+7776FU,T100000000000000000000"))
    assertThrows[RecordFormatException](MoveBuilderCsa.parseCsaString("+7776FU,T-1"))
  }
  it must "restore moves" in forAll(MoveGen.movesCsaFormat) { m =>
    MoveBuilderCsa.parseCsaString(m.toCsaString) mustBe m
  }

  "Move#toSfenString" must "describe the move" in {
    movesForTestSfen map (_.toSfenString) mustBe sfenForTest
  }
  "Move#parseSfenString" must "succeed in normal cases" in {
    sfenForTest map { c => MoveBuilderSfen.parseSfenString(c) } mustBe movesForTestSfen
  }
  it must "throw an exception in error cases" in {
    assertThrows[RecordFormatException](MoveBuilderSfen.parseSfenString(""))
    assertThrows[RecordFormatException](MoveBuilderSfen.parseSfenString(" "))
    assertThrows[RecordFormatException](MoveBuilderSfen.parseSfenString("x" * 1000))
    assertThrows[RecordFormatException](MoveBuilderSfen.parseSfenString("7g76"))
    assertThrows[RecordFormatException](MoveBuilderSfen.parseSfenString("7g7F"))
    assertThrows[RecordFormatException](MoveBuilderSfen.parseSfenString("L*8b+"))
    assertThrows[RecordFormatException](MoveBuilderSfen.parseSfenString("9i1a-"))
    assertThrows[RecordFormatException](MoveBuilderSfen.parseSfenString("K*1a"))
  }
  it must "restore moves" in forAll(MoveGen.movesSfenFormat) { m =>
    MoveBuilderSfen.parseSfenString(m.toSfenString) mustBe m
  }

  "Move#toKifString" must "describe the move" in {
    movesForTestKif map (_.toKifString) mustBe kifForTest
  }
  "Move#parseKifString" must "succeed in normal cases" in {
    kifForTest map { c => MoveBuilderKif.parseKifString(c) } mustBe movesForTestKif

    MoveBuilderKif.parseKifString("６五桂打   ( 0:3/)") mustBe MoveBuilderKifHand(P65, KNIGHT, Some(3))
    MoveBuilderKif.parseKifString("６五桂打   ( 0: 3/)") mustBe MoveBuilderKifHand(P65, KNIGHT, Some(3))
    MoveBuilderKif.parseKifString("６五桂打 (0:3/)") mustBe MoveBuilderKifHand(P65, KNIGHT, Some(3))
    MoveBuilderKif.parseKifString("同　銀(43)   (00:00/00:00:00)") mustBe MoveBuilderKifBoard(P43, None, SILVER, promote = false, Some(0))
  }
  it must "return None in error cases" in {
    assertThrows[RecordFormatException](MoveBuilderKif.parseKifString(""))
    assertThrows[RecordFormatException](MoveBuilderKif.parseKifString(" "))
    assertThrows[RecordFormatException](MoveBuilderKif.parseKifString("x" * 1000))
    assertThrows[RecordFormatException](MoveBuilderKif.parseKifString("３四歩"))
    assertThrows[RecordFormatException](MoveBuilderKif.parseKifString("３四歩 (33)"))
    assertThrows[RecordFormatException](MoveBuilderKif.parseKifString("３四歩(33) (0)"))
    assertThrows[RecordFormatException](MoveBuilderKif.parseKifString("３四歩(33) (0:0)"))
    assertThrows[RecordFormatException](MoveBuilderKif.parseKifString("３四歩(33) (0:0/0)"))
    assertThrows[RecordFormatException](MoveBuilderKif.parseKifString("３四歩(33) (0:0/0:0)"))
    assertThrows[RecordFormatException](MoveBuilderKif.parseKifString("３四歩(33) (0:0/0:0:a)"))
  }
  it must "restore moves" in forAll(MoveGen.movesKifFormat) { m =>
    MoveBuilderKif.parseKifString(m.toKifString) mustBe m
  }

  "Move#toJapaneseNotationString" must "describe the move" in {
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
    ).map(xs => State.parseCsaString(xs.mkString("\n")))

    MoveBuilderSfen.parseSfenString("7g7f").toMove(State.HIRATE).get.toJapaneseNotationString mustBe "７六歩"

    MoveBuilderCsa.parseCsaString("+9382KI").toMove(states(0)).get.toJapaneseNotationString mustBe "８二金上"
    MoveBuilderCsa.parseCsaString("+7282KI").toMove(states(0)).get.toJapaneseNotationString mustBe "８二金寄"
    MoveBuilderCsa.parseCsaString("+4332KI").toMove(states(1)).get.toJapaneseNotationString mustBe "３二金上"
    MoveBuilderCsa.parseCsaString("+3132KI").toMove(states(1)).get.toJapaneseNotationString mustBe "３二金引"
    MoveBuilderCsa.parseCsaString("+5655KI").toMove(states(2)).get.toJapaneseNotationString mustBe "５五金上"
    MoveBuilderCsa.parseCsaString("+4555KI").toMove(states(2)).get.toJapaneseNotationString mustBe "５五金寄"
    MoveBuilderCsa.parseCsaString("+8988GI").toMove(states(3)).get.toJapaneseNotationString mustBe "８八銀上"
    MoveBuilderCsa.parseCsaString("+7788GI").toMove(states(3)).get.toJapaneseNotationString mustBe "８八銀引"
    MoveBuilderCsa.parseCsaString("+4938GI").toMove(states(4)).get.toJapaneseNotationString mustBe "３八銀上"
    MoveBuilderCsa.parseCsaString("+2738GI").toMove(states(4)).get.toJapaneseNotationString mustBe "３八銀引"

    MoveBuilderCsa.parseCsaString("+9281KI").toMove(states(5)).get.toJapaneseNotationString mustBe "８一金左"
    MoveBuilderCsa.parseCsaString("+7281KI").toMove(states(5)).get.toJapaneseNotationString mustBe "８一金右"
    MoveBuilderCsa.parseCsaString("+3222KI").toMove(states(6)).get.toJapaneseNotationString mustBe "２二金左"
    MoveBuilderCsa.parseCsaString("+1222KI").toMove(states(6)).get.toJapaneseNotationString mustBe "２二金右"
    MoveBuilderCsa.parseCsaString("+6556GI").toMove(states(7)).get.toJapaneseNotationString mustBe "５六銀左"
    MoveBuilderCsa.parseCsaString("+4556GI").toMove(states(7)).get.toJapaneseNotationString mustBe "５六銀右"
    MoveBuilderCsa.parseCsaString("+8978KI").toMove(states(8)).get.toJapaneseNotationString mustBe "７八金左"
    MoveBuilderCsa.parseCsaString("+7978KI").toMove(states(8)).get.toJapaneseNotationString mustBe "７八金直"
    MoveBuilderCsa.parseCsaString("+3938GI").toMove(states(9)).get.toJapaneseNotationString mustBe "３八銀直"
    MoveBuilderCsa.parseCsaString("+2938GI").toMove(states(9)).get.toJapaneseNotationString mustBe "３八銀右"

    MoveBuilderCsa.parseCsaString("+6352KI").toMove(states(10)).get.toJapaneseNotationString mustBe "５二金左"
    MoveBuilderCsa.parseCsaString("+5352KI").toMove(states(10)).get.toJapaneseNotationString mustBe "５二金直"
    MoveBuilderCsa.parseCsaString("+4352KI").toMove(states(10)).get.toJapaneseNotationString mustBe "５二金右"
    MoveBuilderCsa.parseCsaString("+7988TO").toMove(states(11)).get.toJapaneseNotationString mustBe "８八と右"
    MoveBuilderCsa.parseCsaString("+8988TO").toMove(states(11)).get.toJapaneseNotationString mustBe "８八と直"
    MoveBuilderCsa.parseCsaString("+9988TO").toMove(states(11)).get.toJapaneseNotationString mustBe "８八と左上"
    MoveBuilderCsa.parseCsaString("+9888TO").toMove(states(11)).get.toJapaneseNotationString mustBe "８八と寄"
    MoveBuilderCsa.parseCsaString("+8788TO").toMove(states(11)).get.toJapaneseNotationString mustBe "８八と引"
    MoveBuilderCsa.parseCsaString("+2928GI").toMove(states(12)).get.toJapaneseNotationString mustBe "２八銀直"
    MoveBuilderCsa.parseCsaString("+1728GI").toMove(states(12)).get.toJapaneseNotationString mustBe "２八銀右"
    MoveBuilderCsa.parseCsaString("+3928GI").toMove(states(12)).get.toJapaneseNotationString mustBe "２八銀左上"
    MoveBuilderCsa.parseCsaString("+3728GI").toMove(states(12)).get.toJapaneseNotationString mustBe "２八銀左引"

    MoveBuilderCsa.parseCsaString("+9182RY").toMove(states(13)).get.toJapaneseNotationString mustBe "８二竜引"
    MoveBuilderCsa.parseCsaString("+8482RY").toMove(states(13)).get.toJapaneseNotationString mustBe "８二竜上"
    MoveBuilderCsa.parseCsaString("+2343RY").toMove(states(14)).get.toJapaneseNotationString mustBe "４三竜寄"
    MoveBuilderCsa.parseCsaString("+5243RY").toMove(states(14)).get.toJapaneseNotationString mustBe "４三竜引"
    MoveBuilderCsa.parseCsaString("+5535RY").toMove(states(15)).get.toJapaneseNotationString mustBe "３五竜左"
    MoveBuilderCsa.parseCsaString("+1535RY").toMove(states(15)).get.toJapaneseNotationString mustBe "３五竜右"
    MoveBuilderCsa.parseCsaString("+9988RY").toMove(states(16)).get.toJapaneseNotationString mustBe "８八竜左"
    MoveBuilderCsa.parseCsaString("+8988RY").toMove(states(16)).get.toJapaneseNotationString mustBe "８八竜右"
    MoveBuilderCsa.parseCsaString("+2817RY").toMove(states(17)).get.toJapaneseNotationString mustBe "１七竜左"
    MoveBuilderCsa.parseCsaString("+1917RY").toMove(states(17)).get.toJapaneseNotationString mustBe "１七竜右"

    MoveBuilderCsa.parseCsaString("+9182UM").toMove(states(18)).get.toJapaneseNotationString mustBe "８二馬左"
    MoveBuilderCsa.parseCsaString("+8182UM").toMove(states(18)).get.toJapaneseNotationString mustBe "８二馬右"
    MoveBuilderCsa.parseCsaString("+9585UM").toMove(states(19)).get.toJapaneseNotationString mustBe "８五馬寄"
    MoveBuilderCsa.parseCsaString("+6385UM").toMove(states(19)).get.toJapaneseNotationString mustBe "８五馬引"
    MoveBuilderCsa.parseCsaString("+1112UM").toMove(states(20)).get.toJapaneseNotationString mustBe "１二馬引"
    MoveBuilderCsa.parseCsaString("+3412UM").toMove(states(20)).get.toJapaneseNotationString mustBe "１二馬上"
    MoveBuilderCsa.parseCsaString("+9977UM").toMove(states(21)).get.toJapaneseNotationString mustBe "７七馬左"
    MoveBuilderCsa.parseCsaString("+5977UM").toMove(states(21)).get.toJapaneseNotationString mustBe "７七馬右"
    MoveBuilderCsa.parseCsaString("+4729UM").toMove(states(22)).get.toJapaneseNotationString mustBe "２九馬左"
    MoveBuilderCsa.parseCsaString("+1829UM").toMove(states(22)).get.toJapaneseNotationString mustBe "２九馬右"

    MoveBuilderCsa.parseCsaString("-5766KA").toMove(states(23)).get.toJapaneseNotationString mustBe "６六角引不成"
    MoveBuilderCsa.parseCsaString("-5766UM").toMove(states(23)).get.toJapaneseNotationString mustBe "６六角引成"
    MoveBuilderCsa.parseCsaString("-6168KY").toMove(states(23)).get.toJapaneseNotationString mustBe "６八香不成"
    MoveBuilderCsa.parseCsaString("-6168NY").toMove(states(23)).get.toJapaneseNotationString mustBe "６八香成"
    MoveBuilderCsa.parseCsaString("-0068KY").toMove(states(23)).get.toJapaneseNotationString mustBe "６八香打"
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
      ).mkString("\n")
    ).map(State.parseCsaString)

    MoveBuilderCsa.parseCsaString("+7776FU").toMove(State.HIRATE).get.toWesternNotationString mustBe "P-7f"
    MoveBuilderCsa.parseCsaString("-3334FU").toMove(State.HIRATE.copy(turn = WHITE)).get.toWesternNotationString mustBe "P-3d"
    MoveBuilderCsa.parseCsaString("+5352UM").toMove(states(0)).get.toWesternNotationString mustBe "+B-5b"
    MoveBuilderCsa.parseCsaString("+0033KY").toMove(states(0)).get.toWesternNotationString mustBe "L*3c"
    MoveBuilderCsa.parseCsaString("+5563KE").toMove(states(0)).get.toWesternNotationString mustBe "N-6c="
    MoveBuilderCsa.parseCsaString("+5563NK").toMove(states(0)).get.toWesternNotationString mustBe "N-6c+"
    MoveBuilderCsa.parseCsaString("+5543KE").toMove(states(0)).get.toWesternNotationString mustBe "N5e-4c="
    MoveBuilderCsa.parseCsaString("+5543NK").toMove(states(0)).get.toWesternNotationString mustBe "N5e-4c+"
    MoveBuilderCsa.parseCsaString("+9283GI").toMove(states(0)).get.toWesternNotationString mustBe "S9bx8c="
    MoveBuilderCsa.parseCsaString("+9283NG").toMove(states(0)).get.toWesternNotationString mustBe "S9bx8c+"
    MoveBuilderCsa.parseCsaString("+9483GI").toMove(states(0)).get.toWesternNotationString mustBe "S9dx8c="
    MoveBuilderCsa.parseCsaString("+9483NG").toMove(states(0)).get.toWesternNotationString mustBe "S9dx8c+"
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
      "+").mkString("\n"))

    MoveBuilderSfen.parseSfenString("7g7f").toMove(s1) mustBe Some(Move(BLACK, Some(P77), P76, PAWN, false, false, None, None, true, None))
  }

  "MoveBuilderSfen#apply" must "create instances" in {
    MoveBuilderSfen(Left(P55), P33, false) mustBe MoveBuilderSfenBoard(P55, P33, false)
    MoveBuilderSfen(Left(P55), P33, true) mustBe MoveBuilderSfenBoard(P55, P33, true)
    MoveBuilderSfen(Right(Hand(BP)), P33, false) mustBe MoveBuilderSfenHand(PAWN, P33)
  }
  it must "throw an error when promote is true and from is in hand" in {
    assertThrows[IllegalArgumentException](MoveBuilderSfen(Right(Hand(BP)), P33, true))
  }

  "MoveBuilderKif#toMove" must "return move" in {
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
      "+").mkString("\n"))

    MoveBuilderKif.parseKifString("７六歩(77) (0:03/)").toMove(s1) mustBe Some(Move(BLACK, Some(P77), P76, PAWN, false, false, None, None, true, Some(3)))
  }

}
