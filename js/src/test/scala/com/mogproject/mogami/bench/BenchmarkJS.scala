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
    benchMateSolver(s1)
    benchMateSolver(s2)
  }
}
