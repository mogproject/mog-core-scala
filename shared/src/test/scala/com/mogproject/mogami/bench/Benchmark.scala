package com.mogproject.mogami.bench

import com.mogproject.mogami._
import com.mogproject.mogami.core.state.{State, StateCache}
import com.mogproject.mogami.mate.MateSolver

/**
  * Shared benchmark utility
  */
case class BenchResult(result: Seq[Double]) {
  def average: Double = if (result.isEmpty) 0.0 else result.sum / result.length / 1000

  def maxValue: Double = result.max / 1000

  def minValue: Double = result.min / 1000

  def print(): Unit = {
    println(f"\n- avg: ${average}%.3fs, min: ${minValue}s, max: ${maxValue}s\n")
  }
}

trait Benchmark {

//  import com.mogproject.mogami.core.state.StateCache.Implicits._

  val benchmarkCount = 3
  val attackRepeat = 10000

  protected def withBenchmark(thunk: => Unit): BenchResult = {
    val ret = (1 to benchmarkCount).map { n =>
      val start = System.currentTimeMillis()
      thunk
      (System.currentTimeMillis() - start).toDouble
    }
    BenchResult(ret)
  }

  def benchGameLoading(sfen: String): Unit = {
    println(s"benchGameLoading: sfen=${sfen}")

    withBenchmark {
      StateCache.withCache { implicit cache =>
        val g = Game.parseSfenString(sfen)
      }
    }.print()
  }

  def benchAttack(piece: Piece, from: Option[Square], allOcc: BitBoard, myPawnOcc: BitBoard): Unit = {
    println(s"benchAttack: repeat=${attackRepeat}, piece=${piece}, from=${from}, allOcc=${allOcc.toOctalString}, myPawnOcc=${myPawnOcc.toOctalString}")

    withBenchmark {
      var i = 0
      while (i < attackRepeat) {
        Attack.get(piece, from, allOcc, myPawnOcc)
        i += 1
      }
    }.print()
  }

  def benchMakeMove(state: State, move: Move): Unit = {
    println("benchMakeMove")

    withBenchmark {
      var i = 0
      while (i < 10000) {
        state.makeMove(move)
        i += 1
      }
    }.print()
  }

  def benchLegalMoves(state: State, move: Move): Unit = {
    println("benchLegalMoves")

    withBenchmark {
      var i = 0
      while (i < 1000) {
        state.makeMove(move).get.legalMoves(None)
        i += 1
      }
    }.print()
  }

  def benchToSfenString(state: State): Unit = {
    println("benchToSfenString")

    withBenchmark {
      var i = 0
      while (i < 10000) {
        state.toSfenString
        i += 1
      }
    }.print()
  }

  def benchMateSolver(state: State): Unit = {
    println("benchMateSolver")

    var ret: Option[Seq[Move]] = None
    withBenchmark {
      ret = MateSolver.solve(state, timeLimitMillis = 2 * 60 * 1000)
    }.print()
    println(ret.map(_.map(_.toJapaneseNotationString)))
  }

//
//  def benchCalcAttackBB(state: State): Unit = {
//    println("benchCalcAttackBB")
//
//    withBenchmark {
//      var i = 0
//      while (i < 10000) {
//        state.calculateAttackBBOnBoard
//        i += 1
//      }
//    }.print()
//  }
//
//  def benchCalcAttackBBDiff(state: State, move: MoveBuilderSfen): Unit = {
//    val m = move.toMove(state).get
//    val newOcc = state.makeMove(m).get.occupancy
//
//    println("benchCalcAttackBBDiff")
//
//    withBenchmark {
//      var i = 0
//      while (i < 10000) {
//        state.getUpdatedAttackBBOnBoard(m, newOcc)
//        i += 1
//      }
//    }.print()
//  }
}
