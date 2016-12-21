package com.mogproject.mogami.core

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, MustMatchers}
import com.mogproject.mogami.core.Player.{BLACK, WHITE}
import com.mogproject.mogami.core.SquareConstant._
import com.mogproject.mogami.core.Ptype._

class MoveSpec extends FlatSpec with MustMatchers with GeneratorDrivenPropertyChecks {
  val movesForTestCsa = Seq(
    Move(Square(7, 7), Square(7, 6), Some(BLACK), Some(Ptype.PAWN), None),
    Move(Square(9, 9), Square(1, 1), Some(WHITE), Some(Ptype.PBISHOP), None),
    Move(Square.HAND, Square(8, 2), Some(BLACK), Some(Ptype.LANCE), None)
  )
  val movesForTestSfen = Seq(
    Move(Square(7, 7), Square(7, 6), None, None, Some(false)),
    Move(Square(9, 9), Square(1, 1), None, None, Some(true)),
    Move(Square.HAND, Square(8, 2), None, Some(Ptype.LANCE), Some(false))
  )
  val csaForTest = Seq("+7776FU", "-9911UM", "+0082KY")
  val sfenForTest = Seq("7g7f", "9i1a+", "L*8b")

  "Move#parseCsaString" must "succeed in normal cases" in {
    csaForTest map { c => Move.parseCsaString(c) } must be(movesForTestCsa map (Some(_)))
  }
  it must "return None in error cases" in {
    Move.parseCsaString("") must be(None)
    Move.parseCsaString(" ") must be(None)
    Move.parseCsaString("x" * 1000) must be(None)
    Move.parseCsaString("=7776FU") must be(None)
    Move.parseCsaString("+0176FU") must be(None)
    Move.parseCsaString("+7770FU") must be(None)
    Move.parseCsaString("+7776FO") must be(None)
    Move.parseCsaString("+7776FUU") must be(None)
    Move.parseCsaString("+0000FU") must be(None)
    Move.parseCsaString("+7777FU") must be(None)
    Move.parseCsaString("+7700FU") must be(None)
  }
  it must "restore moves" in forAll(MoveGen.movesCsaFormat) { m =>
    Move.parseCsaString(m.toCsaString) must be(Some(m))
  }

  "Move#parseSfenString" must "succeed in normal cases" in {
    sfenForTest map { c => Move.parseSfenString(c) } must be(movesForTestSfen map (Some(_)))
  }
  it must "return None in error cases" in {
    Move.parseSfenString("") must be(None)
    Move.parseSfenString(" ") must be(None)
    Move.parseSfenString("x" * 1000) must be(None)
    Move.parseSfenString("7g76") must be(None)
    Move.parseSfenString("7g7F") must be(None)
    Move.parseSfenString("L*8b+") must be(None)
    Move.parseSfenString("9i1a-") must be(None)
    Move.parseSfenString("K*1a") must be(None)
  }
  it must "restore moves" in forAll(MoveGen.movesSfenFormat) { m =>
    Move.parseSfenString(m.toSfenString) must be(Some(m))
  }

  "Move#toCsaString" must "describes the move" in {
    movesForTestCsa map (_.toCsaString) must be(csaForTest)
  }

  "Move#toSfenString" must "describes the move" in {
    movesForTestSfen map (_.toSfenString) must be(sfenForTest)
  }

  "ExtendedMove#fromMove" must "return extended move" in {
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

      ExtendedMove.fromMove(Move.parseSfenString("7g7f").get, s1) mustBe Some(ExtendedMove(BLACK, P77, P76, PAWN, false, None, true))
  }
}
