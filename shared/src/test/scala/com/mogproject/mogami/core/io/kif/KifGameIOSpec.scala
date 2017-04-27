package com.mogproject.mogami.core.io.kif

import com.mogproject.mogami.core.Player.{BLACK, WHITE}
import com.mogproject.mogami.core.Ptype.{KING, PAWN}
import com.mogproject.mogami.core.Square
import com.mogproject.mogami.core.SquareConstant._
import com.mogproject.mogami.core.game.{Branch, Game, GameInfo}
import com.mogproject.mogami.core.move._
import com.mogproject.mogami.core.state.{State, StateCache}
import com.mogproject.mogami.core.state.StateCache.Implicits._
import com.mogproject.mogami.core.state.StateConstant.HIRATE
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, MustMatchers}

class KifGameIOSpec extends FlatSpec with MustMatchers with GeneratorDrivenPropertyChecks {

  object TestKifGameReader extends KifGameReader

  object TestKifBranchReader extends KifBranchReader

  def createGame(initialState: State,
                 moves: Vector[Move] = Vector.empty,
                 gameInfo: GameInfo = GameInfo(),
                 finalAction: Option[SpecialMove] = None)
                (implicit stateCache: StateCache): Game =
    Game(Branch(stateCache.set(initialState), 0, moves, finalAction), Vector.empty, gameInfo)

  val hirateState = Seq(
    "手合割：平手",
    "先手：",
    "後手：",
    "",
    "手数----指手----消費時間--"
  )

  "KifBranchWriter#toKifString" must "describe comments" in {
    val hirate = Branch()
    val hirate2 = hirate.makeMove(MoveBuilderSfenBoard(P27, P26, false)).get.makeMove(MoveBuilderSfenBoard(P51, P42, false)).get
    val hirate3 = Branch(hirate2.lastState.hash, 2).makeMove(MoveBuilderSfenBoard(P77, P76, false)).get

    hirate.toKifString mustBe ""

    hirate.updateComment(0, "comment 0").toKifString mustBe Seq(
      "*comment 0"
    ).mkString("\n")

    hirate.updateComment(0, "com\nment\n 0\nx ").toKifString mustBe Seq(
      "*com",
      "*ment",
      "* 0",
      "*x "
    ).mkString("\n")

    hirate2.updateComment(0, "comment 0").updateComment(2, "comment 2").toKifString mustBe Seq(
      "*comment 0",
      "   1 ２六歩(27)",
      "   2 ４二玉(51)",
      "*comment 2"
    ).mkString("\n")

    hirate3.updateComment(2, "comment 2").updateComment(3, "comment 3").toKifString mustBe Seq(
      "*comment 2",
      "   3 ７六歩(77)",
      "*comment 3"
    ).mkString("\n")
  }

  "KifGameWriter#toKifString" must "describe special moves" in {
    createGame(HIRATE, finalAction = Some(Resign())).toKifString mustBe (hirateState ++ Seq("   1 投了", "")).mkString("\n")
    createGame(HIRATE, finalAction = Some(Resign(Some(123)))).toKifString mustBe (hirateState ++ Seq("   1 投了 (02:03/)", "")).mkString("\n")
    createGame(HIRATE, finalAction = Some(TimeUp())).toKifString mustBe (hirateState ++ Seq("   1 切れ負け", "")).mkString("\n")
    createGame(HIRATE, finalAction = Some(TimeUp(Some(123)))).toKifString mustBe (hirateState ++ Seq("   1 切れ負け (02:03/)", "")).mkString("\n")
    createGame(HIRATE, finalAction = Some(IllegalMove(
      Move(BLACK, Some(P59), P51, KING, false, false, None, None, false, None, false)
    ))).toKifString mustBe (hirateState ++ Seq("   1 ５一玉(59)", "   2 反則手", "")).mkString("\n")
    createGame(HIRATE, finalAction = Some(IllegalMove(
      Move(BLACK, Some(P59), P51, KING, false, false, None, None, false, Some(123), false)
    ))).toKifString mustBe (hirateState ++ Seq("   1 ５一玉(59) (02:03/)", "   2 反則手", "")).mkString("\n")
  }
  it must "describe branches" in {
    val tr1 = Branch().makeMove(MoveBuilderSfenBoard(P27, P26, false)).get.makeMove(MoveBuilderSfenBoard(P51, P42, false))
      .get.makeMove(MoveBuilderSfenBoard(P26, P25, false)).get
    val br1 = Branch(tr1.history(2), 2).makeMove(MoveBuilderSfenBoard(P77, P76, false)).get.updateComment(3, "comment\n 3")
    val br2 = Branch(tr1.history(1), 1).copy(
      finalAction = Some(IllegalMove(Move(BLACK, Some(Square(76)), Square(4), KING, false, false, None, None, false, None, false))))
    Game(tr1, Vector(br1, br2)).toKifString mustBe Seq(
      "手合割：平手",
      "先手：",
      "後手：",
      "",
      "手数----指手----消費時間--",
      "   1 ２六歩(27)",
      "   2 ４二玉(51)",
      "   3 ２五歩(26)",
      "",
      "",
      "変化：3手",
      "   3 ７六歩(77)",
      "*comment",
      "* 3",
      "",
      "",
      "変化：2手",
      "   2 ５一玉(59)",
      "   3 反則手",
      ""
    ).mkString("\n")

    val tr2 = Branch(HIRATE.hash, offset = 10).makeMove(MoveBuilderSfenBoard(P27, P26, false)).get.makeMove(MoveBuilderSfenBoard(P51, P42, false))
      .get.makeMove(MoveBuilderSfenBoard(P26, P25, false)).get.updateComment(10, "init")
    val br3 = Branch(tr2.history(2), 12).makeMove(MoveBuilderSfenBoard(P77, P76, false)).get.updateComment(13, "comment\n 13")
    val br4 = Branch(tr2.history(1), 11).copy(
      finalAction = Some(IllegalMove(Move(BLACK, Some(Square(76)), Square(4), KING, false, false, None, None, false, None, false))))
    Game(tr2, Vector(br3, br4)).toKifString mustBe Seq(
      "手合割：平手",
      "先手：",
      "後手：",
      "",
      "手数----指手----消費時間--",
      "*init",
      "  11 ２六歩(27)",
      "  12 ４二玉(51)",
      "  13 ２五歩(26)",
      "",
      "",
      "変化：13手",
      "  13 ７六歩(77)",
      "*comment",
      "* 13",
      "",
      "",
      "変化：12手",
      "  12 ５一玉(59)",
      "  13 反則手",
      ""
    ).mkString("\n")
  }

  "KifGameReader#parseMovesKif" must "parse normal moves" in {
    TestKifGameReader.parseMovesKif(HIRATE, List(), None) mustBe Game()

    TestKifGameReader.parseMovesKif(HIRATE, List(
      ("1 ７六歩(77)   ( 0:12/)", 1), ("2 ８四歩(83)   ( 0:13/)", 2)
    ), None) mustBe createGame(HIRATE, Vector(
      Move(BLACK, Some(P77), P76, PAWN, false, false, None, None, false, Some(12)),
      Move(WHITE, Some(P83), P84, PAWN, false, false, None, None, false, Some(13))
    ))
  }
  it must "parse special moves" in {
    TestKifGameReader.parseMovesKif(HIRATE, List(("1 投了", 1)), None) mustBe createGame(HIRATE, finalAction = Some(Resign()))
    TestKifGameReader.parseMovesKif(HIRATE, List(("1 投了   ( 2:03/)", 1)), None) mustBe createGame(HIRATE, finalAction = Some(Resign(Some(123))))
    TestKifGameReader.parseMovesKif(HIRATE, List(("1 切れ負け", 1)), None) mustBe createGame(HIRATE, finalAction = Some(TimeUp()))
    TestKifGameReader.parseMovesKif(HIRATE, List(("1 切れ負け (2:3/1:2:3)", 1)), None) mustBe createGame(HIRATE, finalAction = Some(TimeUp(Some(123))))
    TestKifGameReader.parseMovesKif(HIRATE, List(("1 ５一玉(59)", 1), ("2 反則手", 1)), None) mustBe createGame(HIRATE,
      finalAction = Some(IllegalMove(Move(BLACK, Some(P59), P51, KING, false, false, None, None, false, None, false)))
    )
    TestKifGameReader.parseMovesKif(HIRATE, List(("1 ５一玉(59)   ( 2:03/)", 1), ("1 反則手   ( 0:0/)", 2)), None) mustBe createGame(HIRATE,
      finalAction = Some(IllegalMove(Move(BLACK, Some(P59), P51, KING, false, false, None, None, false, Some(123), false)))
    )
  }

  "KifGameReader#splitBranches" must "split branches" in {
    TestKifGameReader.splitBranchesKif(Nil, Nil, Nil) mustBe Nil
    TestKifGameReader.splitBranchesKif(List(("1 ７六歩(77)   ( 0:12/)", 1)), Nil, Nil) mustBe Vector(List(("1 ７六歩(77)   ( 0:12/)", 1)))
    TestKifGameReader.splitBranchesKif(List(("変化：1手", 1),("1 ７六歩(77)   ( 0:12/)", 1)), Nil, Nil) mustBe
      Vector(Nil, List(("1 ７六歩(77)   ( 0:12/)", 1)))
    TestKifGameReader.splitBranchesKif(List(("1 ７六歩(77)   ( 0:12/)", 1), ("変化：1手", 2),("1 ７六歩(77)   ( 0:12/)", 3)), Nil, Nil) mustBe
      Vector(List(("1 ７六歩(77)   ( 0:12/)", 1)), List(("1 ７六歩(77)   ( 0:12/)", 3)))
  }

  "KifGameReader#parseKifString" must "create games" in {
    val s1 = Seq(
      "N+",
      "N-",
      "P1 *  * -FU-FU * -FU *  * +TO",
      "P2 *  *  * +KY-FU+KI *  * -GI",
      "P3 * +NK * -KE+KA+KI *  *  * ",
      "P4+KY *  *  *  *  * +TO+FU * ",
      "P5 *  * -NK+FU *  * +OU-TO * ",
      "P6+FU+FU-TO-TO+FU * +NK * +FU",
      "P7 * +KY * -HI *  * -NY+TO * ",
      "P8-FU+KI-GI * +KA * -OU-GI * ",
      "P9 *  * -GI-RY-KI *  *  *  * ",
      "P+00FU",
      "P-",
      "+",
      "+3546OU",
      "-4142FU",
      "T4377",
      "+3423TO",
      "-0031KI",
      "+5847KA",
      "-6747HI"
    ).mkString("\n")
    Game.parseKifString(Game.parseCsaString(s1).toKifString).trunk.moves.length mustBe 6
  }

  "KifGameReader#parseKi2String" must "create games" in {
    val s1 = Seq(
      "N+",
      "N-",
      "P1 *  * -FU-FU * -FU *  * +TO",
      "P2 *  *  * +KY-FU+KI *  * -GI",
      "P3 * +NK * -KE+KA+KI *  *  * ",
      "P4+KY *  *  *  *  * +TO+FU * ",
      "P5 *  * -NK+FU *  * +OU-TO * ",
      "P6+FU+FU-TO-TO+FU * +NK * +FU",
      "P7 * +KY * -HI *  * -NY+TO * ",
      "P8-FU+KI-GI * +KA * -OU-GI * ",
      "P9 *  * -GI-RY-KI *  *  *  * ",
      "P+00FU",
      "P-",
      "+",
      "+3546OU",
      "-4142FU",
      "T4377",
      "+3423TO",
      "-0031KI",
      "+5847KA",
      "-6747HI"
    ).mkString("\n")
    Game.parseKi2String(Game.parseCsaString(s1).toKi2String).trunk.moves.length mustBe 6
  }
  "KifGameReader#splitMovesKi2" must "create games" in {
    TestKifGameReader.splitMovesKi2(Seq(
      ("▲４六玉 △４二歩 ▲２三と △３一金 ▲４七角 △同飛不成", 1),
      ("▲４六玉 △４二歩 ▲２三と △３一金 ▲４七角 △同飛不成", 2),
      ("▲４六玉", 3)
    )) mustBe List(
      ("▲４六玉", 1), ("△４二歩", 1), ("▲２三と", 1), ("△３一金", 1), ("▲４七角", 1), ("△同飛不成", 1),
      ("▲４六玉", 2), ("△４二歩", 2), ("▲２三と", 2), ("△３一金", 2), ("▲４七角", 2), ("△同飛不成", 2),
      ("▲４六玉", 3)
    )

  }

  "KifBranchReader#parseKifString" must "parse moves" in {
    TestKifBranchReader.parseKifString(List(
      ("1 ７六歩(77)   ( 0:12/)", 1), ("2 ８四歩(83)   ( 0:13/)", 2)
    ), HIRATE) mustBe Branch(HIRATE.hash, 0, Vector(
      Move(BLACK, Some(P77), P76, PAWN, false, false, None, None, false, Some(12)),
      Move(WHITE, Some(P83), P84, PAWN, false, false, None, None, false, Some(13))
    ))
  }

  "KifBranchReader#parseComments" must "parse comments" in {
    TestKifBranchReader.parseComments(TestKifBranchReader.convertLines(Seq(("", 1)))) mustBe Map.empty
    TestKifBranchReader.parseComments(TestKifBranchReader.convertLines(Seq(("*cmt", 1)))) mustBe Map(0 -> "cmt")
    TestKifBranchReader.parseComments(TestKifBranchReader.convertLines(Seq(
      ("*cmt ", 1),
      ("* x ", 1),
      ("***a ", 1),
      ("* b", 1)
    ))) mustBe Map(0 -> "cmt \n x \n**a \n b")
    TestKifBranchReader.parseComments(TestKifBranchReader.convertLines(Seq(
      ("   1 ２六歩(27)", 1),
      ("   2 ４二玉(51)", 2),
      ("   3 ２五歩(26)", 3)
    ))) mustBe Map.empty
    TestKifBranchReader.parseComments(TestKifBranchReader.convertLines(Seq(
      ("*対局開始", 1),
      ("*", 2),
      ("   1 ２六歩(27)", 3),
      ("*すごい", 4),
      ("   2 ４二玉(51)", 5),
      ("*よい", 6),
      ("*よい", 7),
      ("   3 ２五歩(26)", 8),
      ("*かてる", 9)
    ))) mustBe Map(
      0 -> "対局開始\n",
      1 -> "すごい",
      2 -> "よい\nよい",
      3 -> "かてる"
    )
    TestKifBranchReader.parseComments(TestKifBranchReader.convertLines(Seq(
      ("*途中対局開始", 1),
      ("*", 2),
      ("  12 ２六歩(27)", 3),
      ("*すごい", 4),
      ("  13 ４二玉(51)", 5),
      ("*よい", 6),
      ("*よい", 7),
      ("  14 ２五歩(26)", 8),
      ("*かてる", 9)
    ))) mustBe Map(
      11 -> "途中対局開始\n",
      12 -> "すごい",
      13 -> "よい\nよい",
      14 -> "かてる"
    )
  }

}
