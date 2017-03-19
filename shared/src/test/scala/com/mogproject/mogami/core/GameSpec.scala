package com.mogproject.mogami.core

import org.scalatest.{FlatSpec, MustMatchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import com.mogproject.mogami._
import com.mogproject.mogami.core.SquareConstant._
import com.mogproject.mogami.core.StateConstant._
import com.mogproject.mogami.core.Game.GameStatus._

class GameSpec extends FlatSpec with MustMatchers with GeneratorDrivenPropertyChecks {

  val stateHirateInv = State(WHITE, HIRATE.board, HIRATE.hand)
  val stateEmpty = State(BLACK, Map(), State.EMPTY_HANDS)
  val stateEmptyInv = State(WHITE, Map(), State.EMPTY_HANDS)

  val dataForTest = Seq(
    Game(stateEmpty, Vector(), GameInfo()),
    Game(HIRATE, Vector(
      Move(BLACK, Some(P77), P76, PAWN, false, false, None, None, false)
    ), GameInfo(
      Map(
        'formatVersion -> "2.2",
        'blackName -> "NAKAHARA",
        'whiteName -> "YONENAGA",
        'event -> "13th World Computer Shogi Championship",
        'site -> "KAZUSA ARC",
        'startTime -> "2003/05/03 10:30:00",
        'endTime -> "2003/05/03 11:11:05",
        'timeLimit -> "00:25+00",
        'opening -> "YAGURA"
      ))),
    Game(HIRATE, Vector(
      Move(BLACK, Some(P77), P76, PAWN, false, false, None, None, false, Some(50)),
      Move(WHITE, Some(P33), P34, PAWN, false, false, None, None, false, Some(1)),
      Move(BLACK, Some(P88), P22, PBISHOP, true, false, None, Some(BISHOP), false, Some(12)),
      Move(WHITE, Some(P31), P22, SILVER, false, true, None, Some(PBISHOP), false, Some(100)),
      Move(BLACK, None, P33, BISHOP, false, false, None, None, true, Some(10)),
      Move(WHITE, Some(P51), P62, KING, false, false, None, None, false, Some(2)),
      Move(BLACK, Some(P33), P55, PBISHOP, true, false, None, None, false, Some(3))
    ), GameInfo(
      Map('formatVersion -> "", 'blackName -> "B", 'whiteName -> "W", 'event -> "", 'site -> "",
        'startTime -> "", 'endTime -> "", 'timeLimit -> "", 'opening -> "")
    ))
  )
  val csaForTest = Seq(
    "P1 *  *  *  *  *  *  *  *  * \n" +
      "P2 *  *  *  *  *  *  *  *  * \n" +
      "P3 *  *  *  *  *  *  *  *  * \n" +
      "P4 *  *  *  *  *  *  *  *  * \n" +
      "P5 *  *  *  *  *  *  *  *  * \n" +
      "P6 *  *  *  *  *  *  *  *  * \n" +
      "P7 *  *  *  *  *  *  *  *  * \n" +
      "P8 *  *  *  *  *  *  *  *  * \n" +
      "P9 *  *  *  *  *  *  *  *  * \n" +
      "P+\n" +
      "P-\n" +
      "+",
    "V2.2\n" +
      "N+NAKAHARA\nN-YONENAGA\n$EVENT:13th World Computer Shogi Championship\n$SITE:KAZUSA " +
      "ARC\n$START_TIME:2003/05/03 10:30:00\n$END_TIME:2003/05/03 11:11:05\n$TIME_LIMIT:00:25+00\n$OPENING:YAGURA\n" +
      "P1-KY-KE-GI-KI-OU-KI-GI-KE-KY\n" +
      "P2 * -HI *  *  *  *  * -KA * \n" +
      "P3-FU-FU-FU-FU-FU-FU-FU-FU-FU\n" +
      "P4 *  *  *  *  *  *  *  *  * \n" +
      "P5 *  *  *  *  *  *  *  *  * \n" +
      "P6 *  *  *  *  *  *  *  *  * \n" +
      "P7+FU+FU+FU+FU+FU+FU+FU+FU+FU\n" +
      "P8 * +KA *  *  *  *  * +HI * \n" +
      "P9+KY+KE+GI+KI+OU+KI+GI+KE+KY\n" +
      "P+\n" +
      "P-\n" +
      "+\n" +
      "+7776FU",
    "V\nN+B\nN-W\n$EVENT:\n$SITE:\n$START_TIME:\n$END_TIME:\n$TIME_LIMIT:\n$OPENING:\n" +
      "P1-KY-KE-GI-KI-OU-KI-GI-KE-KY\n" +
      "P2 * -HI *  *  *  *  * -KA * \n" +
      "P3-FU-FU-FU-FU-FU-FU-FU-FU-FU\n" +
      "P4 *  *  *  *  *  *  *  *  * \n" +
      "P5 *  *  *  *  *  *  *  *  * \n" +
      "P6 *  *  *  *  *  *  *  *  * \n" +
      "P7+FU+FU+FU+FU+FU+FU+FU+FU+FU\n" +
      "P8 * +KA *  *  *  *  * +HI * \n" +
      "P9+KY+KE+GI+KI+OU+KI+GI+KE+KY\n" +
      "P+\n" +
      "P-\n" +
      "+\n" +
      "+7776FU,T50\n" +
      "-3334FU,T1\n" +
      "+8822UM,T12\n" +
      "-3122GI,T100\n" +
      "+0033KA,T10\n" +
      "-5162OU,T2\n" +
      "+3355UM,T3"
  )
  val sfenForTest = Seq(
    "9/9/9/9/9/9/9/9/9 b - 0",
    "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 0 7g7f",
    "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 0 7g7f 3c3d 8h2b+ 3a2b B*3c 5a6b 3c5e+"
  )
  val kifForTest = Seq(
    """後手：
      |後手の持駒：なし
      |  ９ ８ ７ ６ ５ ４ ３ ２ １
      |+---------------------------+
      || ・ ・ ・ ・ ・ ・ ・ ・ ・|一
      || ・ ・ ・ ・ ・ ・ ・ ・ ・|二
      || ・ ・ ・ ・ ・ ・ ・ ・ ・|三
      || ・ ・ ・ ・ ・ ・ ・ ・ ・|四
      || ・ ・ ・ ・ ・ ・ ・ ・ ・|五
      || ・ ・ ・ ・ ・ ・ ・ ・ ・|六
      || ・ ・ ・ ・ ・ ・ ・ ・ ・|七
      || ・ ・ ・ ・ ・ ・ ・ ・ ・|八
      || ・ ・ ・ ・ ・ ・ ・ ・ ・|九
      |+---------------------------+
      |先手：
      |先手の持駒：なし
      |手数----指手----消費時間--""".stripMargin,
    """手合割：平手
      |先手：NAKAHARA
      |後手：YONENAGA
      |手数----指手----消費時間--
      |   1 ７六歩(77)""".stripMargin,
    """手合割：平手
      |先手：B
      |後手：W
      |手数----指手----消費時間--
      |   1 ７六歩(77) (00:50/)
      |   2 ３四歩(33) (00:01/)
      |   3 ２二角成(88) (00:12/)
      |   4 同　銀(31) (01:40/)
      |   5 ３三角打 (00:10/)
      |   6 ６二玉(51) (00:02/)
      |   7 ５五角成(33) (00:03/)""".stripMargin
  )

  "Game#toCsaString" must "describe some games" in {
    dataForTest.map(_.toCsaString) zip csaForTest foreach { case (a, b) => a must be(b) }
  }
  "Game#parseCsaString" must "work in normal cases" in {
    csaForTest.map(Game.parseCsaString) zip dataForTest.map(Some(_)) foreach { case (a, b) => a must be(b) }
    Game.parseCsaString("+") must be(Some(Game(stateEmpty, Vector(), GameInfo())))
    Game.parseCsaString("-") must be(Some(Game(stateEmptyInv, Vector(), GameInfo())))

    Game.parseCsaString("PI\n-\n-5152OU,T2345\n+5958OU") must be(Some(Game(stateHirateInv, Vector(
      Move(WHITE, Some(P51), P52, KING, false, false, None, None, false, Some(2345)),
      Move(BLACK, Some(P59), P58, KING, false, false, None, None, false)
    ), GameInfo())))
    Game.parseCsaString("N-yyy\n" +
      "P1-KY-KE-GI-KI-OU-KI-GI-KE-KY\n" +
      "P2 * -HI *  *  *  *  * -KA * \n" +
      "P3-FU-FU-FU-FU-FU-FU-FU-FU-FU\n" +
      "P4 *  *  *  *  *  *  *  *  * \n" +
      "P5 *  *  *  *  *  *  *  *  * \n" +
      "P6 *  *  *  *  *  *  *  *  * \n" +
      "P7+FU+FU+FU+FU+FU+FU+FU+FU+FU\n" +
      "P8 * +KA *  *  *  *  * +HI * \n" +
      "P9+KY+KE+GI+KI+OU+KI+GI+KE+KY\n" +
      "P+\n" +
      "P-\n" +
      "-\n-5152OU,T2345\n+5958OU") must be(Some(Game(stateHirateInv, Vector(
      Move(WHITE, Some(P51), P52, KING, false, false, None, None, false, Some(2345)),
      Move(BLACK, Some(P59), P58, KING, false, false, None, None, false)
    ), GameInfo(Map('whiteName -> "yyy")))))
    Game.parseCsaString("V2.2\nN+x\nN-y\n$OPENING:AIGAKARI\n+") must be(Some(Game(stateEmpty, Vector(), GameInfo(
      Map('formatVersion -> "2.2", 'blackName -> "x", 'whiteName -> "y", 'opening -> "AIGAKARI")))))
    Game.parseCsaString("V2.2\nN+x\nN-y\n$OPENING:AIGAKARI\n-") must be(Some(Game(State(
      WHITE, stateEmpty.board, stateEmpty.hand), Vector(), GameInfo(
      Map('formatVersion -> "2.2", 'blackName -> "x", 'whiteName -> "y", 'opening -> "AIGAKARI")))))
    Game.parseCsaString("V2.2\n-") must be(Some(Game(stateEmptyInv, Vector(), GameInfo(Map('formatVersion -> "2.2")))))
    Game.parseCsaString("V2.2\n$EVENT:event name\nN-white name\nPI\n-\n-5152OU,T2345\n+5958OU") must be(Some(Game(
      stateHirateInv,
      Vector(
        Move(WHITE, Some(P51), P52, KING, false, false, None, None, false, Some(2345)),
        Move(BLACK, Some(P59), P58, KING, false, false, None, None, false)
      ),
      GameInfo(Map('formatVersion -> "2.2", 'event -> "event name", 'whiteName -> "white name")))))
  }
  it must "return None when in error cases" in {
    Game.parseCsaString("") must be(None)
    Game.parseCsaString(" ") must be(None)
    Game.parseCsaString("x" * 1000) must be(None)
    Game.parseCsaString("x\n" * 1000) must be(None)
    Game.parseCsaString("$") must be(None)
    Game.parseCsaString("V\n") must be(None)
    Game.parseCsaString("V1\nP+00OU\n+") must be(None)
    Game.parseCsaString("PI\n+\nN+\n+7776FU") must be(None)
  }
  it must "return None when game info is invalid" in {
    Game.parseCsaString("$XXX:XXX\nPI\n-\n-5152OU,T2345\n+5958OU") must be(None)
    Game.parseCsaString("N+xxx\nN=yyy\nPI\n-\n-5152OU,T2345\n+5958OU") must be(None)
  }
  it must "return None when initial state is invalid" in {
    Game.parseCsaString("N+xxx\nN-yyy\nPI") must be(None)
    Game.parseCsaString("N+xxx\nN-yyy\nPI\n+7776FU") must be(None)
    Game.parseCsaString("N+xxx\nN-yyy\nPI\nPI\nPI") must be(None)
    Game.parseCsaString("N+xxx\nN-yyy\nPI\nP\nP") must be(None)
    Game.parseCsaString("N+xxx\nN-yyy\nPI\nP+00FU\n-\n-5152OU,T2345\n+5958OU") must be(None)
  }
  it must "return None when moves are invalid" in {
    Game.parseCsaString("N+xxx\nN-yyy\nPI\n+\n+1234AB") must be(None)
    Game.parseCsaString("N+xxx\nN-yyy\nPI\n+\n+7776FU,TT") must be(None)
    Game.parseCsaString("N+xxx\nN-yyy\nPI\n+\n+7776FU\n+7675FU") must be(None)
    Game.parseCsaString("N+xxx\nN-yyy\nPI\n+\n+7776FU\n-3334KI") must be(None)
    Game.parseCsaString("N+xxx\nN-yyy\nPI\n-\n+7776FU") must be(None)
  }
  it must "restore games" in forAll(GameGen.games, minSuccessful(10)) { g =>
    Game.parseCsaString(g.toCsaString) must be(Some(g))
  }
  it must "ignore comments" in {
    Game.parseCsaString("PI;-;'comment;-5152OU,T2345\n'comment\n+5958OU") must be(Some(Game(stateHirateInv, Vector(
      Move(WHITE, Some(P51), P52, KING, false, false, None, None, false, Some(2345)),
      Move(BLACK, Some(P59), P58, KING, false, false, None, None, false)
    ), GameInfo())))
  }

  "Game#toSfenString" must "describe some games" in {
    dataForTest.map(_.toSfenString) zip sfenForTest foreach { case (a, b) => a must be(b) }
  }
  "Game#parseSfenString" must "create games in normal cases" in {
    sfenForTest.map(Game.parseSfenString) zip dataForTest.map(g =>
      Some(g.copy(gameInfo = GameInfo(), moves = g.moves.map(_.copy(elapsedTime = None))))
    ) foreach { case (a, b) =>
      a must be(b)
    }
  }
  it must "return None in error cases" in {
    Game.parseSfenString("") must be(None)
    Game.parseSfenString(" ") must be(None)
    Game.parseSfenString("x" * 1000) must be(None)
    Game.parseSfenString("x\n" * 1000) must be(None)
    Game.parseSfenString("$") must be(None)
    Game.parseSfenString("9/9/9/9/9/9/9/9/9 B -") must be(None)
    Game.parseSfenString("9/9/9/9/9/9/9/9/9 B ") must be(None)
    Game.parseSfenString("9/9/9/9/9/9/9/9/9 b - xxxx") must be(None)
  }
  it must "restore games" in forAll(GameGen.games, minSuccessful(10)) { g =>
    val s = g.toSfenString
    Game.parseSfenString(s).map(_.toSfenString) must be(Some(s))
  }

  "Game#toKifString" must "describe some games" in {
    dataForTest.map(_.toKifString) zip kifForTest foreach { case (a, b) => a must be(b) }
  }

  "Game#status" must "return Playing when playing" in {
    Game().status mustBe Playing
    Game.parseCsaString("PI\n+\n+7776FU\n-5152OU\n+8833UM").get.status mustBe Playing
    Game.parseCsaString(Seq(
      "P1 *  *  *  *  *  *  *  * -OU",
      "P2 *  *  *  *  *  *  * -FU * ",
      "P3 *  *  *  *  *  *  * +RY * ",
      "P4 *  *  *  *  *  *  *  *  * ",
      "P5 *  *  *  *  *  *  *  *  * ",
      "P6 *  *  *  *  *  *  *  *  * ",
      "P7 *  *  *  *  *  *  *  *  * ",
      "P8 *  *  *  *  *  *  *  *  * ",
      "P9 *  *  *  *  *  *  *  *  * ",
      "P+00FU",
      "+",
      "+0012FU"
    ).mkString("\n")).get.status mustBe Playing
  }
  it must "return Mated when mated" in {
    Game.parseCsaString(Seq(
      "P1 *  *  *  *  *  *  *  * -OU",
      "P2 *  *  *  *  *  *  *  * +KI",
      "P3 *  *  *  *  *  *  *  * +FU",
      "P4 *  *  *  *  *  *  *  *  * ",
      "P5 *  *  *  *  *  *  *  *  * ",
      "P6 *  *  *  *  *  *  *  *  * ",
      "P7 *  *  *  *  *  *  *  *  * ",
      "P8 *  *  *  *  *  *  *  *  * ",
      "P9 *  *  *  *  *  *  *  *  * ",
      "-"
    ).mkString("\n")).get.status mustBe Mated
    Game.parseCsaString(Seq(
      "P1 *  *  *  *  *  *  *  *  * ",
      "P2 *  *  *  *  *  *  *  *  * ",
      "P3 *  *  *  *  *  *  *  *  * ",
      "P4 *  *  *  *  *  *  *  *  * ",
      "P5 *  *  *  *  *  *  *  *  * ",
      "P6 *  *  *  *  *  *  *  *  * ",
      "P7 *  *  *  *  *  *  * -GI-KI",
      "P8 *  *  *  *  *  *  *  *  * ",
      "P9 *  *  *  *  *  *  *  * +OU",
      "-",
      "-2718NG"
    ).mkString("\n")).get.status mustBe Mated
    Game.parseCsaString(Seq(
      "P1 *  *  *  *  *  *  *  * -OU",
      "P2 *  *  *  *  *  *  *  *  * ",
      "P3 *  *  *  *  *  *  * +RY+FU",
      "P4 *  *  *  *  *  *  *  *  * ",
      "P5 *  *  *  *  *  *  *  *  * ",
      "P6 *  *  *  *  *  *  *  *  * ",
      "P7 *  *  *  *  *  *  *  *  * ",
      "P8 *  *  *  *  *  *  *  *  * ",
      "P9 *  *  *  *  *  *  *  *  * ",
      "+",
      "+1312FU"
    ).mkString("\n")).get.status mustBe Mated

    // dropping pawn but not a check
    Game.parseCsaString(Seq(
      "P1 *  *  *  *  *  *  *  * -OU",
      "P2 *  *  *  *  *  *  *  *  * ",
      "P3 *  *  *  *  *  *  * +RY * ",
      "P4 *  *  *  *  *  *  *  *  * ",
      "P5 *  *  *  *  *  *  *  *  * ",
      "P6 *  *  *  *  *  *  *  *  * ",
      "P7 *  *  *  *  *  *  *  *  * ",
      "P8 *  *  *  *  *  *  *  *  * ",
      "P9 *  *  *  *  *  *  *  *  * ",
      "P+00FU",
      "+",
      "+0013FU"
    ).mkString("\n")).get.status mustBe Mated
  }
  it must "return Illegal when perpetual check" in {
    Game.parseCsaString(Seq(
      "P1 *  *  *  *  *  *  *  * -OU",
      "P2 *  *  *  *  *  *  *  *  * ",
      "P3 *  *  *  *  *  *  * +GI+FU",
      "P4 *  *  *  *  *  *  * +FU * ",
      "P5 *  *  *  *  *  *  *  *  * ",
      "P6 *  *  *  *  *  *  *  *  * ",
      "P7 *  *  *  *  *  *  *  *  * ",
      "P8 *  *  *  *  *  *  *  *  * ",
      "P9 *  *  *  *  *  *  *  *  * ",
      "+",
      "+2312GI",
      "-1122OU",
      "+1223GI",
      "-2211OU",
      "+2312GI",
      "-1122OU",
      "+1223GI",
      "-2211OU",
      "+2312GI",
      "-1122OU",
      "+1223GI",
      "-2211OU",
      "+2312GI"
    )).get.status mustBe PerpetualCheck
    Game.parseCsaString(Seq(
      "P1 *  *  *  *  *  *  *  *  * ",
      "P2 *  *  *  *  *  *  *  *  * ",
      "P3 *  *  *  *  *  *  *  *  * ",
      "P4 *  *  *  *  *  *  *  *  * ",
      "P5 *  *  *  *  *  *  *  *  * ",
      "P6 *  *  *  *  *  *  *  *  * ",
      "P7 *  *  *  *  *  *  * -GI-FU",
      "P8 *  *  *  *  *  *  *  *  * ",
      "P9 *  *  *  *  *  *  *  * +OU",
      "-",
      "-2718GI",
      "+1928OU",
      "-1827GI",
      "+2819OU",
      "-2718GI",
      "+1928OU",
      "-1827GI",
      "+2819OU",
      "-2718GI",
      "+1928OU",
      "-1827GI",
      "+2819OU",
      "-2718GI"
    )).get.status mustBe PerpetualCheck
  }
  it must "return Illegal when uchifuzume" in {
    Game.parseCsaString(Seq(
      "P1 *  *  *  *  *  *  *  * -OU",
      "P2 *  *  *  *  *  *  *  *  * ",
      "P3 *  *  *  *  *  *  * +RY * ",
      "P4 *  *  *  *  *  *  *  *  * ",
      "P5 *  *  *  *  *  *  *  *  * ",
      "P6 *  *  *  *  *  *  *  *  * ",
      "P7 *  *  *  *  *  *  *  *  * ",
      "P8 *  *  *  *  *  *  *  *  * ",
      "P9 *  *  *  *  *  *  *  *  * ",
      "P+00FU",
      "+",
      "+0012FU"
    )).get.status mustBe Uchifuzume
  }
  it must "return Drawn when repetition" in {
    Game.parseCsaString(Seq(
      "P1 *  *  *  *  *  *  *  * -OU",
      "P2 *  *  *  *  *  *  *  *  * ",
      "P3 *  *  *  *  *  *  * +GI+FU",
      "P4 *  *  *  *  *  *  * +FU * ",
      "P5 *  *  *  *  *  *  *  *  * ",
      "P6 *  *  *  *  *  *  *  *  * ",
      "P7 *  *  *  *  *  *  *  *  * ",
      "P8 *  *  *  *  *  *  *  *  * ",
      "P9 *  *  *  *  *  *  *  *  * ",
      "+",
      "+2312GI",
      "-1122OU",
      "+1223GI",
      "-2211OU",
      "+2312GI",
      "-1122OU",
      "+1223GI",
      "-2211OU",
      "+2312GI",
      "-1122OU",
      "+1223GI",
      "-2211OU",
      "+2334GI",
      "-1121OU",
      "+3423GI",
      "-2111OU"
    )).get.status mustBe Drawn
  }
}
