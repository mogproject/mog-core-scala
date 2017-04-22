package com.mogproject.mogami.core.io.sfen

import com.mogproject.mogami.core.Player.BLACK
import com.mogproject.mogami.core.Ptype.KING
import com.mogproject.mogami.core.Square
import com.mogproject.mogami.core.move.{IllegalMove, Move, Resign}
import com.mogproject.mogami.core.state.StateCache.Implicits._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, MustMatchers}

class SfenGameIOSpec extends FlatSpec with MustMatchers with GeneratorDrivenPropertyChecks {

  object TestSfenGameReader extends SfenGameReader

  "SfenGameReader#parseSfenExtendedGame" must "parse games" in {
    val s1 = SfenExtendedGame(
      SfenExtendedBranch(
        "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 0 2g2f 5a4b 2f2e",
        None,
        Map(0 -> "initial\ncomment", 1 -> "mv1", 2 -> "mv2", 3 -> "mv3")
      ),
      Vector.empty
    )
    val g1 = TestSfenGameReader.parseSfenExtendedGame(s1)
    g1.trunk.moves.length mustBe 3
    g1.trunk.comments mustBe Map(0 -> "initial\ncomment", 1 -> "mv1", 2 -> "mv2", 3 -> "mv3")
    g1.branches.length mustBe 0

    val s2 = SfenExtendedGame(
      SfenExtendedBranch(
        "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 0 2g2f 5a4b 2f2e",
        None,
        Map(0 -> "initial\ncomment", 1 -> "mv1", 2 -> "mv2", 3 -> "mv3")
      ),
      Vector(
        SfenExtendedBranch(
          "2 7g7f 4b3b",
          None,
          Map(2 -> "m2", 3 -> "m3")
        ),
        SfenExtendedBranch(
          "2 5g5f 4b3b",
          Some("r"),
          Map(2 -> "v2", 3 -> "v3")
        ),
        SfenExtendedBranch(
          "0",
          Some("i 5i5a"),
          Map.empty
        )
      )
    )
    val g2 = TestSfenGameReader.parseSfenExtendedGame(s2)
    g2.trunk.moves.length mustBe 3
    g2.trunk.comments mustBe Map(0 -> "initial\ncomment", 1 -> "mv1", 2 -> "mv2", 3 -> "mv3")
    g2.branches.length mustBe 3
    g2.branches(0).moves.length mustBe 2
    g2.branches(0).finalAction mustBe None
    g2.branches(0).comments mustBe Map(2 -> "m2", 3 -> "m3")
    g2.branches(1).moves.length mustBe 2
    g2.branches(1).finalAction mustBe Some(Resign())
    g2.branches(1).comments mustBe Map(2 -> "v2", 3 -> "v3")
    g2.branches(2).finalAction mustBe Some(IllegalMove(Move(BLACK, Some(Square(76)), Square(4), KING, false, false, None, None, false, None, false)))
  }

}
