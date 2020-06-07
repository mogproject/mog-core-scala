package com.mogproject.mogami.core.attack

import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

import com.mogproject.mogami.core._
import com.mogproject.mogami.core.PieceConstant._

class DropAttackSpec extends AnyFlatSpec with Matchers with ScalaCheckDrivenPropertyChecks  {
  object TestDropAttack extends DropAttack
  
  "DropAttack#getDropAttack" must "work for black pawn" in {
    val pawnOcc = BitBoard.seq(
      """
        |--------- --------- --------- --------- --------- --------- --------- ---------
        |--------- ********* --------- --------- *-------- --------- --------- ----*----
        |--------- --------- --------- --------- -*------- --------- --------- ---------
        |--------- --------- --------- --------- --*------ --------- --------- ---------
        |--------- --------- --------- --------- ---*----- --------- --**----- ---------
        |--------- --------- --------- --------- -----*--- *---**--- --------* --*---*--
        |--------- --------- ********* --------- ------*-- -*------- *-------- ---------
        |--------- --------- --------- --------- -------*- --------- --------- ---------
        |--------- --------- --------- ********* --------* --------- --------- *-------*
      """.stripMargin
    )
    val expected = BitBoard.seq(
      """
        |--------- --------- --------- --------- --------- --------- --------- ---------
        |********* --------- --------- --------- ----*---- --**--*** -*--****- -*-*-*-*-
        |********* --------- --------- --------- ----*---- --**--*** -*--****- -*-*-*-*-
        |********* --------- --------- --------- ----*---- --**--*** -*--****- -*-*-*-*-
        |********* --------- --------- --------- ----*---- --**--*** -*--****- -*-*-*-*-
        |********* --------- --------- --------- ----*---- --**--*** -*--****- -*-*-*-*-
        |********* --------- --------- --------- ----*---- --**--*** -*--****- -*-*-*-*-
        |********* --------- --------- --------- ----*---- --**--*** -*--****- -*-*-*-*-
        |********* --------- --------- --------- ----*---- --**--*** -*--****- -*-*-*-*-
      """.stripMargin)

    pawnOcc.length must be(expected.length)
    pawnOcc zip expected foreach { case (a, b) => (a, TestDropAttack.getDropAttack(BP, a)) must be((a, b))}
  }
  it must "work for white pawn" in {
    val pawnOcc = BitBoard.seq(
      """
        |--------- ********* --------- --------- *-------- --------- --------- *-------*
        |--------- --------- --------- --------- -*------- --------- --------- ----*----
        |--------- --------- ********* --------- --*------ --------- --------- ---------
        |--------- --------- --------- --------- ---*----- --------- --------- ---------
        |--------- --------- --------- --------- -----*--- --------- --**----- ---------
        |--------- --------- --------- --------- ------*-- *---**--- --------* --*---*--
        |--------- --------- --------- --------- -------*- -*------- *-------- ---------
        |--------- --------- --------- ********* --------* --------- --------- ---------
        |--------- --------- --------- --------- --------- --------- --------- ---------
      """.stripMargin
    )
    val expected = BitBoard.seq(
      """
        |********* --------- --------- --------- ----*---- --**--*** -*--****- -*-*-*-*-
        |********* --------- --------- --------- ----*---- --**--*** -*--****- -*-*-*-*-
        |********* --------- --------- --------- ----*---- --**--*** -*--****- -*-*-*-*-
        |********* --------- --------- --------- ----*---- --**--*** -*--****- -*-*-*-*-
        |********* --------- --------- --------- ----*---- --**--*** -*--****- -*-*-*-*-
        |********* --------- --------- --------- ----*---- --**--*** -*--****- -*-*-*-*-
        |********* --------- --------- --------- ----*---- --**--*** -*--****- -*-*-*-*-
        |********* --------- --------- --------- ----*---- --**--*** -*--****- -*-*-*-*-
        |--------- --------- --------- --------- --------- --------- --------- ---------
      """.stripMargin)

    pawnOcc.length must be(expected.length)
    pawnOcc zip expected foreach { case (a, b) => (a, TestDropAttack.getDropAttack(WP, a)) must be((a, b))}
  }
  it must "work for black lance" in {
    TestDropAttack.getDropAttack(BL, BitBoard.full) must be(BitBoard(
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
      """.stripMargin))
  }
  it must "work for white lance" in {
    TestDropAttack.getDropAttack(WL, BitBoard.full) must be(BitBoard(
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
      """.stripMargin))
  }
  it must "work for black knight" in {
    TestDropAttack.getDropAttack(BN, BitBoard.full) must be(BitBoard(
      """
        |---------
        |---------
        |*********
        |*********
        |*********
        |*********
        |*********
        |*********
        |*********
      """.stripMargin))
  }
  it must "work for white knight" in {
    TestDropAttack.getDropAttack(WN, BitBoard.full) must be(BitBoard(
      """
        |*********
        |*********
        |*********
        |*********
        |*********
        |*********
        |*********
        |---------
        |---------
      """.stripMargin))
  }
  it must "work for black silver" in {
    TestDropAttack.getDropAttack(BS, BitBoard.full) must be(BitBoard.full)
  }
  it must "work for white silver" in {
    TestDropAttack.getDropAttack(BS, BitBoard.full) must be(BitBoard.full)
  }
  it must "work for black gold" in {
    TestDropAttack.getDropAttack(BS, BitBoard.full) must be(BitBoard.full)
  }
  it must "work for white gold" in {
    TestDropAttack.getDropAttack(BS, BitBoard.full) must be(BitBoard.full)
  }
  it must "work for black bishop" in {
    TestDropAttack.getDropAttack(BS, BitBoard.full) must be(BitBoard.full)
  }
  it must "work for white bishop" in {
    TestDropAttack.getDropAttack(BS, BitBoard.full) must be(BitBoard.full)
  }
  it must "work for black rook" in {
    TestDropAttack.getDropAttack(BS, BitBoard.full) must be(BitBoard.full)
  }
  it must "work for white rook" in {
    TestDropAttack.getDropAttack(BS, BitBoard.full) must be(BitBoard.full)
  }
}
