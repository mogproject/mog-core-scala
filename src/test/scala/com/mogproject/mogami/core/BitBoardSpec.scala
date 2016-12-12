package com.mogproject.mogami.core

import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, MustMatchers}
import com.mogproject.mogami.core.Player.{BLACK, WHITE}
import com.mogproject.mogami.core.SquareConstant._


class BitBoardSpec extends FlatSpec with MustMatchers with GeneratorDrivenPropertyChecks {

  trait context {
    val bitboards = Seq(BitBoard.empty, BitBoard.full,
      BitBoard(0x1dddddddddddddL, 0x7777777L), BitBoard(0x7fd05ffL, 0x7fd05ffL))
  }

  "BitBoard#equals" must "distinguish some bitboards" in {
    BitBoard.empty.equals(BitBoard.empty) mustBe true
    BitBoard.empty.equals(BitBoard()) mustBe true
    BitBoard.empty.equals(BitBoard(0L, 0L)) mustBe true
    BitBoard.empty.equals(BitBoard(1L, 0L)) mustBe false
    BitBoard.empty.equals(BitBoard(0L, 1L)) mustBe false
    BitBoard.full.equals(BitBoard(0x003fffffffffffffL, 0x0000000007ffffffL)) mustBe true
    BitBoard.full.equals(BitBoard(0x003fffffffffffffL, 0x0000000003ffffffL)) mustBe false
    BitBoard(1L, 2L).equals(BitBoard(1L, 2L)) mustBe true
  }

  "BitBoard#toString" must "describe some bitboards" in new context {
    bitboards.map(_.toString) mustBe Seq(
      "---------\n---------\n---------\n---------\n---------\n---------\n---------\n---------\n---------",
      "*********\n*********\n*********\n*********\n*********\n*********\n*********\n*********\n*********",
      "***-***-*\n-***-***-\n*-***-***\n**-***-**\n***-***-*\n-***-***-\n*-***-***\n**-***-**\n***-***-*",
      "*********\n-*-----*-\n*********\n---------\n---------\n---------\n*********\n-*-----*-\n*********"
    )
  }
  it must "have same length" in forAll(BitBoardGen.bitboards) { bb =>
    bb.toString.length mustBe 89
    bb.toString.count(_ == '\n') mustBe 8
  }

  "BitBoard#get" must "be true if bit is on" in {
    BitBoard.empty.get(0) mustBe false
    BitBoard.empty.get(80) mustBe false
    BitBoard.full.get(0) mustBe true
    BitBoard.full.get(80) mustBe true
  }

  "BitBoard#set" must "turn the bit on" in {
    BitBoard.empty.set(0) mustBe BitBoard(1L, 0L)
    BitBoard.empty.set(80) mustBe BitBoard(0L, 0x4000000L)
    BitBoard.full.set(0) mustBe BitBoard.full
    BitBoard.full.set(80) mustBe BitBoard.full
  }
  it must "be get of true after set the bit on" in forAll(BitBoardGen.bitboards, Gen.choose(0, 80)) { (bb, x) =>
    bb.set(x).get(x) mustBe true
  }

  "BitBoard#reset" must "turn the bit off" in {
    BitBoard.empty.reset(0) mustBe BitBoard.empty
    BitBoard.empty.reset(80) mustBe BitBoard.empty
    BitBoard.full.reset(0) mustBe BitBoard(0x003ffffffffffffeL, 0x0000000007ffffffL)
    BitBoard.full.reset(80) mustBe BitBoard(0x003fffffffffffffL, 0x0000000003ffffffL)
  }

  it must "be get of false after set the bit off" in forAll(BitBoardGen.bitboards, Gen.choose(0, 80)) { (bb, x) =>
    bb.reset(x).get(x) mustBe false
  }

  "BitBoard#isEmpty" must "be true if the bitboard is empty" in {
    BitBoard.empty.isEmpty mustBe true
    BitBoard(0L, 0L).isEmpty mustBe true
    BitBoard(1L, 0L).isEmpty mustBe false
    BitBoard(0L, 1L).isEmpty mustBe false
    BitBoard.full.isEmpty mustBe false
  }

  "BitBoard#count" must "be the number of 1-bits" in new context {
    bitboards.map(_.count) mustBe Seq(0, 81, 61, 40)
  }

  "BitBoard#~" must "flip all bits" in new context {
    bitboards.map(~_) mustBe Seq(
      BitBoard.full, BitBoard.empty, BitBoard(0x22222222222222L, 0x888888L), BitBoard(0x3ffffff802fa00L, 0x2fa00L))
  }

  it must "be recovered after filpping twice" in forAll(BitBoardGen.bitboards) { bb =>
    ~(~bb) mustBe bb
  }

  "BitBoard#&" must "be conjunction of the bits" in {
    BitBoard.empty & BitBoard.empty mustBe BitBoard.empty
    BitBoard.empty & BitBoard.full mustBe BitBoard.empty
    BitBoard.full & BitBoard.full mustBe BitBoard.full
    BitBoard(3L, 0L) & BitBoard(6L, 0L) mustBe BitBoard(2L, 0L)
  }

  it must "keep some properties" in forAll(BitBoardGen.bitboards) { bb =>
    bb & BitBoard.empty mustBe BitBoard.empty
    bb & BitBoard.full mustBe bb
    bb & bb mustBe bb
    bb & ~bb mustBe BitBoard.empty
  }

  "BitBoard#|" must "be disjunction of the bits" in {
    BitBoard.empty | BitBoard.empty mustBe BitBoard.empty
    BitBoard.empty | BitBoard.full mustBe BitBoard.full
    BitBoard.full | BitBoard.full mustBe BitBoard.full
    BitBoard(3L, 0L) | BitBoard(6L, 0L) mustBe BitBoard(7L, 0L)
  }

  it must "keep some properties" in forAll(BitBoardGen.bitboards) { bb =>
    bb | BitBoard.empty mustBe bb
    bb | BitBoard.full mustBe BitBoard.full
    bb | bb mustBe bb
    bb | ~bb mustBe BitBoard.full
  }

  "BitBoard#^" must "be exclusive disjunction of the bits" in {
    BitBoard.empty ^ BitBoard.empty mustBe BitBoard.empty
    BitBoard.empty ^ BitBoard.full mustBe BitBoard.full
    BitBoard.full ^ BitBoard.full mustBe BitBoard.empty
    BitBoard(3L, 0L) ^ BitBoard(6L, 0L) mustBe BitBoard(5L, 0L)
  }

  it must "keep some properties" in forAll(BitBoardGen.bitboards) { bb =>
    bb ^ BitBoard.empty mustBe bb
    bb ^ BitBoard.full mustBe ~bb
    bb ^ bb mustBe BitBoard.empty
    bb ^ ~bb mustBe BitBoard.full
  }

  "BitBoard#indent" must "be work as identity of the position" in {
    BitBoard.ident(0) mustBe BitBoard(1L, 0L)
    BitBoard.ident(53) mustBe BitBoard(0x0020000000000000L, 0L)
    BitBoard.ident(54) mustBe BitBoard(0L, 1L)
    BitBoard.ident(80) mustBe BitBoard(0L, 0x4000000L)
  }

  "BitBoard#shiftRight" must "shift all bits to right" in {
    (0 to 9).foreach {
      BitBoard.empty.shiftRight(_) mustBe BitBoard.empty
    }
    BitBoard.full.shiftRight(0) mustBe BitBoard.full
    BitBoard.full.shiftRight(1) mustBe BitBoard(
      """
        |-********
        |-********
        |-********
        |-********
        |-********
        |-********
        |-********
        |-********
        |-********
      """.stripMargin
    )
    BitBoard.full.shiftRight(9) mustBe BitBoard.empty
    BitBoard(
      """
        |*********
        |-*-----*-
        |*--***-**
        |------*--
        |-*-------
        |--*----*-
        |*******-*
        |-------*-
        |*********
      """.stripMargin).shiftRight(2) mustBe BitBoard(
      """
        |--*******
        |---*-----
        |--*--***-
        |--------*
        |---*-----
        |----*----
        |--*******
        |---------
        |--*******
      """.stripMargin)
  }

  "BitBoard#shiftLeft" must "shift all bits to left" in {
    (0 to 9).foreach {
      BitBoard.empty.shiftLeft(_) mustBe BitBoard.empty
    }
    BitBoard.full.shiftLeft(0) mustBe BitBoard.full
    BitBoard.full.shiftLeft(1) mustBe BitBoard(
      """
        |********-
        |********-
        |********-
        |********-
        |********-
        |********-
        |********-
        |********-
        |********-
      """.stripMargin
    )
    BitBoard.full.shiftLeft(9) mustBe BitBoard.empty
    BitBoard(
      """
        |*********
        |-*-----*-
        |*--***-**
        |------*--
        |-*-------
        |--*----*-
        |*******-*
        |-------*-
        |*********
      """.stripMargin).shiftLeft(2) mustBe BitBoard(
      """
        |*******--
        |-----*---
        |-***-**--
        |----*----
        |---------
        |*----*---
        |*****-*--
        |-----*---
        |*******--
      """.stripMargin)
  }

  "BitBoard#shiftUp" must
    "shift all bits to up" in {
    (0 to 9).foreach {
      BitBoard.empty.shiftUp(_) mustBe BitBoard.empty
    }
    BitBoard.full.shiftUp(0) mustBe BitBoard.full
    BitBoard.full.shiftUp(1) mustBe BitBoard(
      """
        |*********
        |*********
        |*********
        |*********
        |*********
        |*********
        |*********
        |*********
        |---------
      """.stripMargin
    )
    BitBoard.full.shiftUp(9) mustBe BitBoard.empty
    BitBoard(
      """
        |*********
        |-*-----*-
        |*--***-**
        |------*--
        |-*-------
        |--*----*-
        |*******-*
        |-------*-
        |*********
      """.stripMargin).shiftUp(2) mustBe BitBoard(
      """
        |*--***-**
        |------*--
        |-*-------
        |--*----*-
        |*******-*
        |-------*-
        |*********
        |---------
        |---------
      """.stripMargin)
  }

  "BitBoard#shiftDown" must "shift all bits to down" in {
    (0 to 9).foreach {
      BitBoard.empty.shiftDown(_) mustBe BitBoard.empty
    }
    BitBoard.full.shiftDown(0) mustBe BitBoard.full
    BitBoard.full.shiftDown(1) mustBe BitBoard(
      """
        |---------
        |*********
        |*********
        |*********
        |*********
        |*********
        |*********
        |*********
        |*********
      """.stripMargin
    )
    BitBoard.full.shiftDown(9) mustBe BitBoard.empty
    BitBoard(
      """
        |*********
        |-*-----*-
        |*--***-**
        |------*--
        |-*-------
        |--*----*-
        |*******-*
        |-------*-
        |*********
      """.stripMargin).shiftDown(2) mustBe BitBoard(
      """
        |---------
        |---------
        |*********
        |-*-----*-
        |*--***-**
        |------*--
        |-*-------
        |--*----*-
        |*******-*
      """.stripMargin
    )
  }

  "BitBoard#flipVertical" must "flip all bits vertically" in {
    BitBoard.empty.flipVertical mustBe BitBoard.empty
    BitBoard.full.flipVertical mustBe BitBoard.full
    BitBoard(
      """
        |*********
        |-*-----*-
        |*--***-**
        |------*--
        |-*-------
        |--*----*-
        |*******-*
        |-------*-
        |*********
      """.stripMargin).flipVertical mustBe BitBoard(
      """
        |*********
        |-------*-
        |*******-*
        |--*----*-
        |-*-------
        |------*--
        |*--***-**
        |-*-----*-
        |*********
      """.stripMargin)
  }

  "BitBoard#mirrorHorizontal" must "mirror all bits horizontally" in {
    BitBoard.empty.mirrorHorizontal mustBe BitBoard.empty
    BitBoard.full.mirrorHorizontal mustBe BitBoard.full
    BitBoard(
      """
        |*********
        |-*-----*-
        |*--***-**
        |------*--
        |-*-------
        |--*----*-
        |*******-*
        |-------*-
        |*********
      """.stripMargin).mirrorHorizontal mustBe BitBoard(
      """
        |*********
        |-*-----*-
        |**-***--*
        |--*------
        |-------*-
        |-*----*--
        |*-*******
        |-*-------
        |*********
      """.stripMargin)
  }

  "BitBoard#spreadAllFile" must "set all file-direction bits" in {
    BitBoard.empty.spreadAllFile mustBe BitBoard.empty
    BitBoard.full.spreadAllFile mustBe BitBoard.full
    BitBoard(
      """
        |--------*
        |---------
        |---------
        |---------
        |---------
        |---------
        |---------
        |---------
        |---------
      """.stripMargin).spreadAllFile mustBe BitBoard(
      """
        |--------*
        |--------*
        |--------*
        |--------*
        |--------*
        |--------*
        |--------*
        |--------*
        |--------*
      """.stripMargin)
    BitBoard(
      """
        |---------
        |---------
        |---------
        |---------
        |---------
        |---------
        |---------
        |---------
        |*--------
      """.stripMargin).spreadAllFile mustBe BitBoard(
      """
        |*--------
        |*--------
        |*--------
        |*--------
        |*--------
        |*--------
        |*--------
        |*--------
        |*--------
      """.stripMargin)
    BitBoard(
      """
        |---------
        |---------
        |---------
        |---*-----
        |---------
        |*-*---*--
        |-*------*
        |---------
        |---------
      """.stripMargin).spreadAllFile mustBe BitBoard(
      """
        |****--*-*
        |****--*-*
        |****--*-*
        |****--*-*
        |****--*-*
        |****--*-*
        |****--*-*
        |****--*-*
        |****--*-*
      """.stripMargin)
    BitBoard(
      """
        |--------*
        |-------*-
        |------*--
        |-----*---
        |----*----
        |---*-----
        |--*------
        |-*-------
        |*--------
      """.stripMargin).spreadAllFile mustBe BitBoard.full
    BitBoard(
      """
        |---------
        |---------
        |---------
        |---------
        |---------
        |*********
        |---------
        |---------
        |---------
      """.stripMargin).spreadAllFile mustBe BitBoard.full
  }
  "BitBoard#seq" must "make bitboard sequence from long-width string sequence" in {
    BitBoard.seq(
      """
        |---------  --------*  *********  ----*----  --------*  *--------  *****-***  -*-*-*-*-  ---**----  *********
        |---------  ---------  ---------  ----*----  -------*-  -*-------  ****---**  ---------  ------**-  *********
        |---------  ---------  ---------  ----*----  ------*--  --*------  ***-----*  -*-*-*-*-  ----*----  *********
        |---------  ---------  ---------  ----*----  -----*---  ---*-----  **-------  ---------  ---**----  *********
        |---------  ---------  ---------  ----*----  ----*----  ----*----  *********  -*-*-*-*-  --*------  *********
        |---------  ---------  ---------  ----*----  ---*-----  -----*---  -*--*--*-  ---------  ---------  *********
        |---------  ---------  ---------  ----*----  --*------  ------*--  --*-*-*--  -*-*-*-*-  -----*---  *********
        |---------  ---------  ---------  ----*----  -*-------  -------*-  ---***---  ---------  --------*  *********
        |---------  ---------  ---------  ----*----  *--------  --------*  ----*----  -*-*-*-*-  *--------  *********
      """.stripMargin) mustBe Seq(BitBoard(
      """
        |---------
        |---------
        |---------
        |---------
        |---------
        |---------
        |---------
        |---------
        |---------
      """.stripMargin), BitBoard(
      """
        |--------*
        |---------
        |---------
        |---------
        |---------
        |---------
        |---------
        |---------
        |---------
      """.stripMargin), BitBoard(
      """
        |*********
        |---------
        |---------
        |---------
        |---------
        |---------
        |---------
        |---------
        |---------
      """.stripMargin), BitBoard(
      """
        |----*----
        |----*----
        |----*----
        |----*----
        |----*----
        |----*----
        |----*----
        |----*----
        |----*----
      """.stripMargin), BitBoard(
      """
        |--------*
        |-------*-
        |------*--
        |-----*---
        |----*----
        |---*-----
        |--*------
        |-*-------
        |*--------
      """.stripMargin), BitBoard(
      """
        |*--------
        |-*-------
        |--*------
        |---*-----
        |----*----
        |-----*---
        |------*--
        |-------*-
        |--------*
      """.stripMargin), BitBoard(
      """
        |*****-***
        |****---**
        |***-----*
        |**-------
        |*********
        |-*--*--*-
        |--*-*-*--
        |---***---
        |----*----
      """.stripMargin), BitBoard(
      """
        |-*-*-*-*-
        |---------
        |-*-*-*-*-
        |---------
        |-*-*-*-*-
        |---------
        |-*-*-*-*-
        |---------
        |-*-*-*-*-
      """.stripMargin), BitBoard(
      """
        |---**----
        |------**-
        |----*----
        |---**----
        |--*------
        |---------
        |-----*---
        |--------*
        |*--------
      """.stripMargin), BitBoard(
      """
        |*********
        |*********
        |*********
        |*********
        |*********
        |*********
        |*********
        |*********
        |*********
      """.stripMargin))
  }
  "BitBoard#attackingThird" must "work with black" in {
    BitBoard.attackingThird(BLACK) mustBe BitBoard(
      """
        |*********
        |*********
        |*********
        |---------
        |---------
        |---------
        |---------
        |---------
        |---------
      """.stripMargin)
  }
  it must "work with white" in {
    BitBoard.attackingThird(WHITE) mustBe BitBoard(
      """
        |---------
        |---------
        |---------
        |---------
        |---------
        |---------
        |*********
        |*********
        |*********
      """.stripMargin)
  }
  "BitBoard#toSet" must "return empty set when bitboard is empty" in {
    BitBoard.empty.toSet mustBe Set.empty[Square]
  }
  it must "return full set when bitboard is full" in {
    BitBoard.full.toSet mustBe Square.BOARD.toSet
  }
  it must "return set of positions" in {
    BitBoard(
      """
        |*--*----*
        |--*--*---
        |-----*---
        |---------
        |---*-----
        |---------
        |--*------
        |---------
        |*-------*
      """).toSet mustBe Set(P11, P19, P42, P43, P61, P65, P72, P77, P91, P99)
  }
  it must "preserve number of bits" in forAll(BitBoardGen.bitboards) { bb =>
    bb.toSet.size mustBe bb.count
  }
}

