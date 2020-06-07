package com.mogproject.mogami.core.io.sfen

import com.mogproject.mogami._
import com.mogproject.mogami.core.game.{Game, GameGen}
import com.mogproject.mogami.core.move.Movement.Upward
import com.mogproject.mogami.core.move.{IllegalMove, Move, Resign}
import com.mogproject.mogami.core.state.StateCache
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class SfenGameIOSpec extends AnyFlatSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  object TestSfenGameReader extends SfenGameReader

  "SfenGameReader#parseUsenString" must "parse games" in StateCache.withCache { implicit cache =>
    val s1 = "lnsgkgsnl_1r5b1_ppppppppp_9_9_9_PPPPPPPPP_1B5R1_LNSGKGSNL.b.-~0.6y20io5t2."
    val s2 = "lnsgkgsnl_1r5b1_ppppppppp_9_9_9_PPPPPPPPP_1B5R1_LNSGKGSNL.b.-~0.6y20io5t2.~2.7ku1im.~2.7bq1im.r~0..i9i8"

    val g1 = TestSfenGameReader.parseUsenString(s1)
    g1.trunk.moves.length mustBe 3
    g1.branches.length mustBe 0

    val g2 = TestSfenGameReader.parseUsenString(s2)
    g2.trunk.moves.length mustBe 3
    g2.branches.length mustBe 3
    g2.branches(0).moves.length mustBe 2
    g2.branches(0).finalAction mustBe None
    g2.branches(1).moves.length mustBe 2
    g2.branches(1).finalAction mustBe Some(Resign())
    g2.branches(2).finalAction mustBe Some(IllegalMove(Move(BLACK, Some(Square(76)), Square(4), KING, false, false, None, None, false, None, false)))

    val s3 = Seq(
      "lnsgkgsnl_1r5b1_ppppppppp_9_9_9_PPPPPPPPP_1B5R1_LNSGKGSNL.b.-",
      "0.6y236e5t24be9qc0e47ku2jm4o22f281kbek3jm.", // Trunk
      "12.3kk.", // Branch: 1
      "11.05m3jm5ge7pe22w.", // Branch: 2
      "8.8uc1cd9yu.", // Branch: 3
      "3.2jm4o22f281k4be.", // Branch: 4
      "3.2jm4o22f27ku1cx9uw.", // Branch: 5
      "3.2jm4o22f281k0e4bek1a43kk.", // Branch: 6
      "3.0e4.", // Branch: 7
      "0.72m2jm83m.", // Branch: 8
      "0.72m36e83m." // Branch: 9
    ).mkString("~")
    /*
                1       2       3       4       5       6       7       8       9       10      11      12      13      14      15      16
      trunk: +2726FU -8384FU +2625FU -8485FU +6978KI -4132KI +7776FU -3334FU +2524FU -2324FU +2824HI -0023FU +2434HI
      br 1 :                                                                                                 +2426HI
      br 2 :                                                                                         -2133KE +2434HI -8586FU +8786FU -8286HI
      br 3 :                                                                 +8877KA -2277UM +8977KE
      br 4 :                         -3334FU +2524FU -2324FU +2824HI -8485FU
      br 5 :                         -3334FU +2524FU -2324FU +7776FU -2288UM +7988GI
      br 6 :                         -3334FU +2524FU -2324FU +2824HI -4132KI +0023FU -2233KA +2426HI
      br 7 :                         -4132KI
      br 8 : +3736FU -3334FU +2838HI
      br 9 : +3736FU -8384FU +2838HI
     */

    val g3: Game = TestSfenGameReader.parseUsenString(s3)

    g3.trunk.historyHash(12) mustBe g3.branches(0).historyHash(0)
    g3.trunk.historyHash(11) mustBe g3.branches(1).historyHash(0)
    g3.trunk.historyHash(0) mustBe g3.branches(8).historyHash(0)
    g3.branches(3).historyHash(4) mustBe g3.branches(5).historyHash(4)
    g3.branches(3).historyHash(4) mustNot be(g3.branches(4).historyHash(4))

    val s4 = "~0.6y20io5t2."
    val g4 = TestSfenGameReader.parseUsenString(s4)
    g4.trunk.moves.length mustBe 3
    g4.branches.length mustBe 0
    g4.toUsenString mustBe s4
  }
  it must "create moves correctly" in StateCache.withCache { implicit cache =>
    val s1 = "4RB1k1_5s3_7n1_5s1LP_9_7r1_9_9_6K2.b.b4g2s3n3l17p~0.0elbi4bnm3s42em5sk0i51i4bri050bj014k291.~3.1j42em5sk0i5050bs0."
    val g1 = TestSfenGameReader.parseUsenString(s1)

    g1.branches.length mustBe 1
    g1.branches(0).moves mustBe Vector(
      Move(WHITE, Some(Square(12)), Square(20), SILVER, false, true, Some(Upward), Some(KNIGHT), false, None, true),
      Move(BLACK, Some(Square(19)), Square(20), PBISHOP, false, true, None, Some(SILVER), true, None, true),
      Move(WHITE, Some(Square(46)), Square(28), ROOK, false, false, None, Some(LANCE), false, None, true),
      Move(BLACK, Some(Square(4)), Square(2), PROOK, true, false, None, Some(LANCE), true, None, true),
      Move(WHITE, Some(Square(1)), Square(9), KING, false, false, None, None, false, None, true),
      Move(BLACK, None, Square(18), SILVER, false, false, None, None, true, None, true)
    )
  }
  it must "restore games" in StateCache.withCache { implicit cache =>
    forAll(GameGen.games, minSuccessful(10)) { g =>
      val s = g.toUsenString
      Game.parseUsenString(s).toUsenString mustBe s
    }
  }
  it must "create games with the free mode" in StateCache.withCache { implicit cache =>
    val s1 = "~0.7ku8s42i90ia."
    val g1 = TestSfenGameReader.parseUsenString(s1, isFreeMode = true)

    g1.trunk.moves.length mustBe 4
  }
  it must "restore free games" in StateCache.withCache { implicit cache =>
    forAll(GameGen.freeGames, minSuccessful(10)) { g =>
      val s = g.toUsenString
      Game.parseUsenString(s, isFreeMode = true).toUsenString mustBe s
    }
  }
}
