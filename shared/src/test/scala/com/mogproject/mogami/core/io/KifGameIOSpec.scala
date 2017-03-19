package com.mogproject.mogami.core.io


import com.mogproject.mogami._
import com.mogproject.mogami.core.SquareConstant._
import com.mogproject.mogami.core.StateConstant.HIRATE
import com.mogproject.mogami.core.move.{IllegalMove, Move, Resign, TimeUp}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, MustMatchers}

class KifGameIOSpec extends FlatSpec with MustMatchers with GeneratorDrivenPropertyChecks {

  object TestKifGameReader extends KifGameReader

  val hirateState = Seq(
    "手合割：平手",
    "先手：",
    "後手：",
    "手数----指手----消費時間--"
  )

  "KifGameWriter#toKifString" must "describe special moves" in {
    Game(HIRATE, finalAction = Some(Resign())).toKifString mustBe (hirateState ++ Seq("   1 投了")).mkString("\n")
    Game(HIRATE, finalAction = Some(Resign(Some(123)))).toKifString mustBe (hirateState ++ Seq("   1 投了 (02:03/)")).mkString("\n")
    Game(HIRATE, finalAction = Some(TimeUp())).toKifString mustBe (hirateState ++ Seq("   1 切れ負け")).mkString("\n")
    Game(HIRATE, finalAction = Some(TimeUp(Some(123)))).toKifString mustBe (hirateState ++ Seq("   1 切れ負け (02:03/)")).mkString("\n")
    Game(HIRATE, finalAction = Some(IllegalMove(
      Move(BLACK, Some(P59), P51, KING, false, false, None, None, false, None, false)
    ))).toKifString mustBe (hirateState ++ Seq("   1 ５一玉(59)", "   2 反則手")).mkString("\n")
    Game(HIRATE, finalAction = Some(IllegalMove(
      Move(BLACK, Some(P59), P51, KING, false, false, None, None, false, Some(123), false)
    ))).toKifString mustBe (hirateState ++ Seq("   1 ５一玉(59) (02:03/)", "   2 反則手")).mkString("\n")
  }

  "KifGameReader#parseMovesKif" must "parse normal moves" in {
    TestKifGameReader.parseMovesKif(List(), None, Some(Game())) mustBe Some(Game())

    TestKifGameReader.parseMovesKif(List(
      "７六歩(77)   ( 0:12/)", "８四歩(83)   ( 0:13/)"
    ), None, Some(Game())) mustBe Some(Game(HIRATE, Vector(
      Move(BLACK, Some(P77), P76, PAWN, false, false, None, None, false, Some(12)),
      Move(WHITE, Some(P83), P84, PAWN, false, false, None, None, false, Some(13))
    )))
  }
  it must "parse special moves" in {
    TestKifGameReader.parseMovesKif(List("投了"), None, Some(Game())) mustBe Some(Game(HIRATE, finalAction = Some(Resign())))
    TestKifGameReader.parseMovesKif(List("投了   ( 2:03/)"), None, Some(Game())) mustBe Some(Game(HIRATE, finalAction = Some(Resign(Some(123)))))
    TestKifGameReader.parseMovesKif(List("切れ負け"), None, Some(Game())) mustBe Some(Game(HIRATE, finalAction = Some(TimeUp())))
    TestKifGameReader.parseMovesKif(List("切れ負け (2:3/1:2:3)"), None, Some(Game())) mustBe Some(Game(HIRATE, finalAction = Some(TimeUp(Some(123)))))
    TestKifGameReader.parseMovesKif(List("５一玉(59)", "反則手"), None, Some(Game())) mustBe Some(Game(HIRATE,
      finalAction = Some(IllegalMove(Move(BLACK, Some(P59), P51, KING, false, false, None, None, false, None, false)))
    ))
    TestKifGameReader.parseMovesKif(List("５一玉(59)   ( 2:03/)", "反則手   ( 0:0/)"), None, Some(Game())) mustBe Some(Game(HIRATE,
      finalAction = Some(IllegalMove(Move(BLACK, Some(P59), P51, KING, false, false, None, None, false, Some(123), false)))
    ))
  }

  "KifGameReader#parseKifString" must "create games" in {
    val s = Seq(
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
    )
    Game.parseKifString(Game.parseCsaString(s).get.toKifString).isDefined mustBe true
  }
}
