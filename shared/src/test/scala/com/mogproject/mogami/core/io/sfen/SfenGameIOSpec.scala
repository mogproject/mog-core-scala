package com.mogproject.mogami.core.io.sfen

import com.mogproject.mogami.core.Player.BLACK
import com.mogproject.mogami.core.Ptype.KING
import com.mogproject.mogami.core.Square
import com.mogproject.mogami.core.game.{Game, GameGen}
import com.mogproject.mogami.core.move.{IllegalMove, Move, Resign}
import com.mogproject.mogami.core.state.StateCache.Implicits._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, MustMatchers}

class SfenGameIOSpec extends FlatSpec with MustMatchers with GeneratorDrivenPropertyChecks {

  object TestSfenGameReader extends SfenGameReader

  "SfenGameReader#parseSfenExtendedGame" must "parse games" in {
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
  }
  it must "restore games" in forAll(GameGen.games, minSuccessful(10)) { g =>
    val s = g.toUsenString
    Game.parseUsenString(s).toUsenString mustBe s
  }

}
