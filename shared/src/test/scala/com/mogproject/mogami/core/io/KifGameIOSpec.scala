package com.mogproject.mogami.core.io


import com.mogproject.mogami._
import com.mogproject.mogami.core.SquareConstant._
import com.mogproject.mogami.core.state.StateConstant.HIRATE
import com.mogproject.mogami.core.move.{IllegalMove, Move, Resign, TimeUp}
import com.mogproject.mogami.core.state.StateCache.Implicits._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, MustMatchers}

class KifGameIOSpec extends FlatSpec with MustMatchers with GeneratorDrivenPropertyChecks {

  object TestKifGameReader extends KifGameReader

  val hirateState = Seq(
    "手合割：平手",
    "先手：",
    "後手：",
    "",
    "手数----指手----消費時間--"
  )

  "KifGameWriter#toKifString" must "describe special moves" in {
    Game(HIRATE, finalAction = Some(Resign())).toKifString mustBe (hirateState ++ Seq("   1 投了", "")).mkString("\n")
    Game(HIRATE, finalAction = Some(Resign(Some(123)))).toKifString mustBe (hirateState ++ Seq("   1 投了 (02:03/)", "")).mkString("\n")
    Game(HIRATE, finalAction = Some(TimeUp())).toKifString mustBe (hirateState ++ Seq("   1 切れ負け", "")).mkString("\n")
    Game(HIRATE, finalAction = Some(TimeUp(Some(123)))).toKifString mustBe (hirateState ++ Seq("   1 切れ負け (02:03/)", "")).mkString("\n")
    Game(HIRATE, finalAction = Some(IllegalMove(
      Move(BLACK, Some(P59), P51, KING, false, false, None, None, false, None, false)
    ))).toKifString mustBe (hirateState ++ Seq("   1 ５一玉(59)", "   2 反則手", "")).mkString("\n")
    Game(HIRATE, finalAction = Some(IllegalMove(
      Move(BLACK, Some(P59), P51, KING, false, false, None, None, false, Some(123), false)
    ))).toKifString mustBe (hirateState ++ Seq("   1 ５一玉(59) (02:03/)", "   2 反則手", "")).mkString("\n")
  }

  "KifGameReader#parseMovesKif" must "parse normal moves" in {
    TestKifGameReader.parseMovesKif(HIRATE, List(), None) mustBe Game()

    TestKifGameReader.parseMovesKif(HIRATE, List(
      ("７六歩(77)   ( 0:12/)", 1), ("８四歩(83)   ( 0:13/)", 2)
    ), None) mustBe Game(HIRATE, Vector(
      Move(BLACK, Some(P77), P76, PAWN, false, false, None, None, false, Some(12)),
      Move(WHITE, Some(P83), P84, PAWN, false, false, None, None, false, Some(13))
    ))
  }
  it must "parse special moves" in {
    TestKifGameReader.parseMovesKif(HIRATE, List(("投了", 1)), None) mustBe Game(HIRATE, finalAction = Some(Resign()))
    TestKifGameReader.parseMovesKif(HIRATE, List(("投了   ( 2:03/)", 1)), None) mustBe Game(HIRATE, finalAction = Some(Resign(Some(123))))
    TestKifGameReader.parseMovesKif(HIRATE, List(("切れ負け", 1)), None) mustBe Game(HIRATE, finalAction = Some(TimeUp()))
    TestKifGameReader.parseMovesKif(HIRATE, List(("切れ負け (2:3/1:2:3)", 1)), None) mustBe Game(HIRATE, finalAction = Some(TimeUp(Some(123))))
    TestKifGameReader.parseMovesKif(HIRATE, List(("５一玉(59)", 1), ("反則手", 1)), None) mustBe Game(HIRATE,
      finalAction = Some(IllegalMove(Move(BLACK, Some(P59), P51, KING, false, false, None, None, false, None, false)))
    )
    TestKifGameReader.parseMovesKif(HIRATE, List(("５一玉(59)   ( 2:03/)", 1), ("反則手   ( 0:0/)", 2)), None) mustBe Game(HIRATE,
      finalAction = Some(IllegalMove(Move(BLACK, Some(P59), P51, KING, false, false, None, None, false, Some(123), false)))
    )
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
    Game.parseKifString(Game.parseCsaString(s1).toKifString).moves.length mustBe 6
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
    Game.parseKi2String(Game.parseCsaString(s1).toKi2String).moves.length mustBe 6
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
}
