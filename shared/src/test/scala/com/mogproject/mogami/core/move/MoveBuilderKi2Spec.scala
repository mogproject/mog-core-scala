package com.mogproject.mogami.core.move

import com.mogproject.mogami.core.Player.{BLACK, WHITE}
import com.mogproject.mogami.core.Ptype._
import com.mogproject.mogami.core.SquareConstant._
import com.mogproject.mogami.core.io.RecordFormatException
import com.mogproject.mogami.core.move.Movement._
import com.mogproject.mogami.core.state.State
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, MustMatchers}

class MoveBuilderKi2Spec extends FlatSpec with MustMatchers with GeneratorDrivenPropertyChecks {
  "MoveBuilderKi2#parseKi2String" must "succeed in normal cases" in {
    MoveBuilderKi2.parseKi2String("▲７六歩") mustBe MoveBuilderKi2(BLACK, Some(P76), PAWN, None, None)
    MoveBuilderKi2.parseKi2String("△同歩") mustBe MoveBuilderKi2(WHITE, None, PAWN, None, None)
    MoveBuilderKi2.parseKi2String("△同成桂") mustBe MoveBuilderKi2(WHITE, None, PKNIGHT, None, None)
    MoveBuilderKi2.parseKi2String("▲７六金右") mustBe MoveBuilderKi2(BLACK, Some(P76), GOLD, Some(Leftwards), None)
    MoveBuilderKi2.parseKi2String("△７六銀右引") mustBe MoveBuilderKi2(WHITE, Some(P76), SILVER, Some(LeftDownward), None)
    MoveBuilderKi2.parseKi2String("△７六成銀右上") mustBe MoveBuilderKi2(WHITE, Some(P76), PSILVER, Some(LeftUpward), None)
    MoveBuilderKi2.parseKi2String("△７六銀右引不成") mustBe MoveBuilderKi2(WHITE, Some(P76), SILVER, Some(LeftDownward), Some(false))
    MoveBuilderKi2.parseKi2String("△７六銀右引成") mustBe MoveBuilderKi2(WHITE, Some(P76), SILVER, Some(LeftDownward), Some(true))
    MoveBuilderKi2.parseKi2String("△同龍上") mustBe MoveBuilderKi2(WHITE, None, PROOK, Some(Upward), None)
    MoveBuilderKi2.parseKi2String("△同竜上") mustBe MoveBuilderKi2(WHITE, None, PROOK, Some(Upward), None)
  }
  it must "throw an exception in error cases" in {
    assertThrows[RecordFormatException](MoveBuilderKi2.parseKi2String(""))
    assertThrows[RecordFormatException](MoveBuilderKi2.parseKi2String(" "))
    assertThrows[RecordFormatException](MoveBuilderKi2.parseKi2String("x" * 1000))
    assertThrows[RecordFormatException](MoveBuilderKi2.parseKi2String("７六歩"))
    assertThrows[RecordFormatException](MoveBuilderKi2.parseKi2String("▲▲７六歩"))
    assertThrows[RecordFormatException](MoveBuilderKi2.parseKi2String("▲７７歩"))
  }
  it must "restore moves" in forAll(MoveGen.movesKi2Format) { m =>
    MoveBuilderKi2.parseKi2String(m.toKi2String) mustBe m
  }

  "MoveBuilderKi2#findMoveFrom" must "return Some in normal cases" in {
    val st1 = State.parseCsaString(Seq(
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
    ).mkString("\n"))
    val st2 = State.parseCsaString(Seq(
      "P1 *  *  *  *  *  *  *  *  * ",
      "P2 *  *  *  *  *  *  *  *  * ",
      "P3 *  *  *  *  *  *  *  *  * ",
      "P4 *  *  *  *  *  *  *  *  * ",
      "P5 *  *  *  *  *  *  *  *  * ",
      "P6 *  *  *  *  *  *  *  *  * ",
      "P7 *  *  *  *  *  * +GI * +GI",
      "P8 *  *  *  *  *  *  *  *  * ",
      "P9 *  *  *  *  *  *  *  *  * ",
      "P+00GI00GI",
      "P-",
      "+"
    ).mkString("\n"))

    MoveBuilderKi2(BLACK, None, SILVER, None, None).findMoveFrom(st1, P16) mustBe Some(Some(P17))
    MoveBuilderKi2(BLACK, None, SILVER, Some(Rightwards), Some(true)).findMoveFrom(st1, P26) mustBe Some(Some(P37))
    MoveBuilderKi2(BLACK, None, SILVER, Some(RightUpward), Some(true)).findMoveFrom(st1, P28) mustBe Some(Some(P39))
    MoveBuilderKi2(BLACK, None, SILVER, None, None).findMoveFrom(st2, P11) mustBe Some(None)
    MoveBuilderKi2(BLACK, None, SILVER, Some(Dropped), None).findMoveFrom(st2, P11) mustBe Some(None)
    MoveBuilderKi2(BLACK, None, SILVER, None, None).findMoveFrom(st2, P16) mustBe Some(Some(P17))
    MoveBuilderKi2(BLACK, None, SILVER, Some(Dropped), None).findMoveFrom(st2, P16) mustBe Some(None)
  }
  it must "return None in error cases" in {
    val st1 = State.parseCsaString(Seq(
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
    ).mkString("\n"))
    val st2 = State.parseCsaString(Seq(
      "P1 *  *  *  *  *  *  *  *  * ",
      "P2 *  *  *  *  *  *  *  *  * ",
      "P3 *  *  *  *  *  *  *  *  * ",
      "P4 *  *  *  *  *  *  *  *  * ",
      "P5 *  *  *  *  *  *  *  *  * ",
      "P6 *  *  *  *  *  *  *  *  * ",
      "P7 *  *  *  *  *  * +GI * +GI",
      "P8 *  *  *  *  *  *  *  *  * ",
      "P9 *  *  *  *  *  *  *  *  * ",
      "P+00GI00GI",
      "P-",
      "+"
    ).mkString("\n"))

    MoveBuilderKi2(BLACK, None, SILVER, None, None).findMoveFrom(st1, P26) mustBe None
    MoveBuilderKi2(BLACK, None, SILVER, None, None).findMoveFrom(st1, P28) mustBe None
    MoveBuilderKi2(BLACK, None, SILVER, Some(Rightwards), Some(true)).findMoveFrom(st1, P28) mustBe None
  }
}
