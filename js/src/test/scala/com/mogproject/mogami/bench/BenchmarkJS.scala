package com.mogproject.mogami.bench

import com.mogproject.mogami._
import com.mogproject.mogami.core.PieceConstant._
import com.mogproject.mogami.core.SquareConstant._
import com.mogproject.mogami.core.move.MoveBuilderSfen


/**
  * Benchmarks for Scala.js
  */
object BenchmarkJS extends scalajs.js.JSApp with Benchmark with TestData {
  def main(): Unit = {
    benchAttack(BP, None, BitBoard.empty, BitBoard.empty)

    benchAttack(BP, Some(P55), BitBoard.empty, BitBoard.empty)

    benchAttack(BPR, Some(P11), BitBoard.empty, BitBoard.empty)
    benchAttack(BPR, Some(P11), BitBoard.full, BitBoard.full)

    benchAttack(BPB, Some(P55), BitBoard.empty, BitBoard.empty)
    benchAttack(BPB, Some(P55), BitBoard.full, BitBoard.full)

    benchGameLoading(recordSfen01)
    benchGameLoading(recordSfen02)
    benchGameLoading(recordSfen03)
//    val s = State.HIRATE
//    val m = MoveBuilderSfen(Left(P77), P76, false).toMove(s).get
//
//    benchMakeMove(s, m)
//    benchLegalMoves(s, m)
//    benchToSfenString(s)
  }
}
