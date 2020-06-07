package com.mogproject.mogami.mate

import com.mogproject.mogami._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

/**
  *
  */
class MateSolverSpec extends AnyFlatSpec with Matchers with ScalaCheckDrivenPropertyChecks {
  "MateSolver#solve" must "return answers" in {

    val s = Seq(
      "4k4/9/4P4/9/9/9/9/9/9 b G2r2b3g4s4n4l17p",
      "9/9/7B1/6R2/3rg4/4k4/9/4PP3/9 b b3g4s4n4l16p",
      "6Rsk/7r1/8+P/9/9/9/9/9/9 b GN2b3g3s3n4l17p",
      "5B1n1/8k/6Rpp/9/9/9/9/9/9 b rb4g4s3n4l16p",
      "4R1lk1/5s3/6+B2/7rP/9/9/9/9/6K2 b Sb4g2s4n3l17p",
      "7ll/6+P1k/6+S2/7pp/7N1/9/9/9/9 b P2r2b4g3s3n2l14p" // involving Uchifuzume
    ).map(State.parseSfenString)

    MateSolver.solve(s(0)).map(_.map(_.toCsaString)) mustBe Some(List("+0052KI"))
    MateSolver.solve(s(1)).map(_.map(_.toCsaString)) mustBe Some(List("+3436HI"))

    Seq(
      Some(List("+0012KI", "-2212HI", "+0023KE")),
      Some(List("+0023KE", "-2223HI", "+0012KI"))
    ).contains(MateSolver.solve(s(2)).map(_.map(_.toCsaString))) mustBe true
    Seq(
      Some(List("+4123KA", "-1211OU", "+0012FU", "-1122OU", "+3332RY")),
      Some(List("+4123KA", "-1222OU", "+3332RY", "-2211OU", "+2312UM")),
      Some(List("+4123KA", "-1222OU", "+3332RY", "-2211OU", "+3212RY"))
    ).contains(MateSolver.solve(s(3)).map(_.map(_.toCsaString))) mustBe true
    MateSolver.solve(s(4)).map(_.map(_.toCsaString)) mustBe Some(List("+5131RY", "-4231GI", "+0012GI", "-2112OU", "+0013KY", "-1221OU", "+1311NY"))
    MateSolver.solve(s(5)).map(_.map(_.toCsaString)) mustBe Some(List("+3322NG", "-2122KY", "+0013FU", "-1223OU", "+3233TO"))
  }
  it must "return empty seq when there is no solution" in {

    val s = Seq(
      "4k4/9/4P4/9/9/9/9/9/9 b 2r2b4g4s4n4l17p",
      "8k/9/9/9/7N1/9/9/9/9 b 2r2b4g4s3n4l18p",
      "8k/9/9/9/9/9/9/9/9 b 2G2r2b2g4s4n4l18p",
      "4B2nk/6G2/5pppp/7N1/9/9/9/9/9 b 2rb3g4s2n4l14p" // involving Uchifuzume
    ).map(State.parseSfenString)

    MateSolver.solve(s(0)) mustBe Some(Nil)
    MateSolver.solve(s(1)) mustBe Some(Nil)
    MateSolver.solve(s(2)) mustBe Some(Nil)
    MateSolver.solve(s(3), timeLimitMillis = 4 * 60 * 1000) mustBe Some(Nil)
  }
  it must "return None when the solver requires more moves or time" in {
    val s = Seq(
      "9/9/7B1/6R2/3rg4/4k4/9/4P4/9 b b3g4s4n4l17p",
      "4RB1k1/5s3/7n1/5s1LP/9/7r1/9/9/6K2 b b4g2s3n3l17p"
    ).map(State.parseSfenString)

    MateSolver.solve(s(0), maxDepth = 3) mustBe None
    MateSolver.solve(s(1), maxDepth = 3) mustBe None
  }
  it must "return the longest answer" in {
    val s = Seq(
      "9/9/6B2/6p2/5n1l1/5p1k1/9/5S+B2/9 b RPr4g3s3n3l15p"
    ).map(State.parseSfenString)

    MateSolver.solve(s(0), timeLimitMillis = 20 * 60 * 1000).map(_.map(_.toJapaneseNotationString)) mustBe Some(List(
      "４四角成", "１五玉", "１六馬", "１四玉", "２五馬", "同玉", "２六飛", "１四玉", "１五歩", "同玉", "１六香"
    ))
  }
}
