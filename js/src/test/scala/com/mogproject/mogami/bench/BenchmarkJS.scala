package com.mogproject.mogami.bench

import com.mogproject.mogami._
import com.mogproject.mogami.core.state.StateConstant._
import com.mogproject.mogami.core.PieceConstant._
import com.mogproject.mogami.core.SquareConstant._
import com.mogproject.mogami.core.move.MoveBuilderSfen
import com.mogproject.mogami.core.state.StateCache


/**
  * Benchmarks for Scala.js
  */
object BenchmarkJS extends scalajs.js.JSApp with Benchmark with TestData {
  def main(): Unit = {
    benchGameLoad()
  }

  def misc(): Unit = {

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
  }

  def benchGameLoad(): Unit = {
    val repeat = 100

    println(s"benchGameLoad: different cache")

    withBenchmark {
      var i = 0
      while (i < repeat) {
        StateCache.withCache { implicit cache =>
          val g = Game.parseUsenString(recordUsen01)
        }
        i += 1
      }
    }.print()


    println(s"benchGameLoad: same cache")

    withBenchmark {
      var i = 0
      StateCache.withCache { implicit cache =>
        while (i < repeat) {
          val g = Game.parseUsenString(recordUsen01)
          i += 1
        }
      }
    }.print()
  }

  def benchMateSolver(): Unit = {
    val s1 = State.parseSfenString("4k4/9/9/9/3+PP4/9/9/9/9 b 4G2r2b4s4n4l16p") // mate in 9
    val s2 = State.parseSfenString("8k/7p1/1r7/5bS2/7N1/9/9/9/9 b RSNLb4g2s2n3l17p") // mate in 7
    val s3 = State.parseSfenString("1+P2Ss2l/1+S5b1/k1p4p1/1p+r1G2n1/p7p/P1N2S3/KPP1PP+pBP/7+r1/LNg6 b GNL2Pgl3p")
    val s4 = State.parseSfenString("4RB1k1/5s3/7n1/5s1LP/9/7r1/9/9/6K2 b b4g2s3n3l17p") // Karolina, mate in 13
    val s5 = State.parseSfenString("k1+P4n1/2L+P2sL1/r4+P+P1P/+BpP+Pl1+Rg1/NP1S+PP+p1g/2L+p1g+P1+P/Ps1G1+P1N1/1sN1P4/B8 b -") // Kemuri
    val s6 = State.parseSfenString("g1+P1k1+P+P+L/1p3P3/+R+p2pp1pl/1NNsg+p2+R/+b+nL+P1+p3/1P3ssP1/2P1+Ps2N/4+P1P1L/+B5G1g b -") // Microcosmos, mate in 1535
    val s7 = State.parseSfenString("5n1k1/5p3/5s1p1/8N/7P1/9/9/9/9 b R2Br4g3s2n4l15p") // mate in 13
    val s8 = State.parseSfenString("5B1n1/8k/6Rpp/9/9/9/9/9/9 b rb4g4s3n4l16p") // mate in 5 involving Uchifuzume
    val s9 = State.parseSfenString("5n1k1/5ps2/7p1/8N/7P1/9/9/9/9 b RBrb4g3s2n4l15p") // mate in 11
    benchMateSolver(s1)
    benchMateSolver(s2)
    //    benchMateSolver(s3)
    benchMateSolver(s4)
    //    benchMateSolver(s5)
    //    benchMateSolver(s6)
    benchMateSolver(s7)
    benchMateSolver(s8)
    benchMateSolver(s9)
  }
}

/*
 * Result
 *

[2017-05-14] Mac (2.9 GHz Intel Core i7)

benchMateSolver

- avg: 1.723s, min: 1.474s, max: 2.19s

Some(List(５二金, 同玉, ５三金, 同玉, ５四と, ６二玉, ６三金, ５一玉, ５二金打))
benchMateSolver

- avg: 9.773s, min: 8.538s, max: 12.113s

Some(List(３一飛, ２一香, １二銀, 同玉, ２四桂, １一玉, １二香))
benchMateSolver

- avg: 74.166s, min: 73.511s, max: 74.967s

Some(List(２三角成, ３一香, ３三桂, 同銀引, 同馬, ２三香, ３一飛成, 同銀, １二銀, 同玉, １三香, ２一玉, １一香成))
benchMateSolver

- avg: 0.069s, min: 0.061s, max: 0.083s

Some(List(２三角不成, ２二玉, ３二飛成, １一玉, １二角成))
benchMateSolver

- avg: 5.287s, min: 4.402s, max: 6.252s

Some(List(３一飛, １二玉, ３二飛成, １三玉, ２四角, １四玉, １五銀, ２五玉, ３五竜, １六玉, ２六竜))

[2017-06-28] Mac (2.9 GHz Intel Core i7): Incremental search

benchMateSolver

- avg: 1.985s, min: 1.713s, max: 2.391s

Some(List(５二金, 同玉, ５三金, 同玉, ５四と, ４二玉, ４三金, ３一玉, ３二金打))
benchMateSolver

- avg: 6.504s, min: 5.963s, max: 7.431s

Some(List(３一飛, ２一桂, １二銀, 同玉, ２四桂, １一玉, １二香))
benchMateSolver

- avg: 54.624s, min: 53.085s, max: 57.543s

Some(List(２三角成, ３一香, ３三桂, 同銀引, 同馬, ２四飛, ３一飛成, 同銀, １二銀, 同玉, １三香, ２一玉, １一香成))
benchMateSolver

- avg: 91.213s, min: 87.138s, max: 97.839s

Some(List(３二角, 同銀, ３一飛, １二玉, ３二飛成, １三玉, ２四角, １四玉, １五銀, ２五玉, ３五竜, １六玉, ２六竜))
benchMateSolver

- avg: 0.090s, min: 0.081s, max: 0.108s

Some(List(２三角不成, １一玉, １二歩, ２二玉, ３二飛成))
benchMateSolver

- avg: 7.914s, min: 7.855s, max: 7.958s

Some(List(３一飛, １二玉, ３二飛成, １三玉, ２四角, １四玉, １五銀, ２五玉, ３五竜, １六玉, ２六竜))

 */