package com.mogproject.mogami.bench

import com.mogproject.mogami._
import com.mogproject.mogami.core.state.StateConstant._
import com.mogproject.mogami.core.PieceConstant._
import com.mogproject.mogami.core.SquareConstant._
import com.mogproject.mogami.core.move.MoveBuilderSfen


/**
  * Benchmarks for Scala.js
  */
object BenchmarkJS extends scalajs.js.JSApp with Benchmark with TestData {
  def main(): Unit = {
//    benchAttack(BP, None, BitBoard.empty, BitBoard.empty)
//
//    benchAttack(BP, Some(P55), BitBoard.empty, BitBoard.empty)
//
//    benchAttack(BPR, Some(P11), BitBoard.empty, BitBoard.empty)
//    benchAttack(BPR, Some(P11), BitBoard.full, BitBoard.full)
//
//    benchAttack(BPB, Some(P55), BitBoard.empty, BitBoard.empty)
//    benchAttack(BPB, Some(P55), BitBoard.full, BitBoard.full)

    if (false) {
      benchGameLoading(recordSfen01)
      benchGameLoading(recordSfen02)
      benchGameLoading(recordSfen03)

      val s = State.HIRATE
      val m = MoveBuilderSfen(Left(P77), P76, false).toMove(s).get

      benchMakeMove(s, m)
      benchLegalMoves(s, m)
      benchToSfenString(s)
    }

//    benchCalcAttackBB(HIRATE)
//    benchCalcAttackBBDiff(HIRATE, MoveBuilderSfen(Left(P77), P76, false))



    val s1 = State.parseSfenString("4k4/9/9/9/3+PP4/9/9/9/9 b 4G2r2b4s4n4l16p") // mate in 9
    val s2 = State.parseSfenString("8k/7p1/1r7/5bS2/7N1/9/9/9/9 b RSNLb4g2s2n3l17p") // mate in 7
    val s3 = State.parseSfenString("1+P2Ss2l/1+S5b1/k1p4p1/1p+r1G2n1/p7p/P1N2S3/KPP1PP+pBP/7+r1/LNg6 b GNL2Pgl3p")
    val s4 = State.parseSfenString("4RB1k1/5s3/7n1/5s1LP/9/7r1/9/9/6K2 b b4g2s3n3l17p") // Karolina, mate in 13
    val s5 = State.parseSfenString("k1+P4n1/2L+P2sL1/r4+P+P1P/+BpP+Pl1+Rg1/NP1S+PP+p1g/2L+p1g+P1+P/Ps1G1+P1N1/1sN1P4/B8 b -") // Kemuri
    val s6 = State.parseSfenString("g1+P1k1+P+P+L/1p3P3/+R+p2pp1pl/1NNsg+p2+R/+b+nL+P1+p3/1P3ssP1/2P1+Ps2N/4+P1P1L/+B5G1g b -") // Microcosmos, mate in 1535
    benchMateSolver(s1)
    benchMateSolver(s2)
//    benchMateSolver(s3)
//    benchMateSolver(s4)
//    benchMateSolver(s5)
//    benchMateSolver(s6)
  }
}
