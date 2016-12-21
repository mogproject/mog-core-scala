package com.mogproject.mogami.core

import org.scalatest.{FlatSpec, MustMatchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

import com.mogproject.mogami._
import com.mogproject.mogami.core.SquareConstant._
import com.mogproject.mogami.core.PieceConstant._

class GameSpec extends FlatSpec with MustMatchers with GeneratorDrivenPropertyChecks {
  val stateHirate = State(BLACK, Map(
    P11 -> WL, P21 -> WN, P31 -> WS, P41 -> WG, P51 -> WK, P61 -> WG, P71 -> WS, P81 -> WN, P91 -> WL,
    P22 -> WB, P82 -> WR,
    P13 -> WP, P23 -> WP, P33 -> WP, P43 -> WP, P53 -> WP, P63 -> WP, P73 -> WP, P83 -> WP, P93 -> WP,
    P17 -> BP, P27 -> BP, P37 -> BP, P47 -> BP, P57 -> BP, P67 -> BP, P77 -> BP, P87 -> BP, P97 -> BP,
    P28 -> BR, P88 -> BB,
    P19 -> BL, P29 -> BN, P39 -> BS, P49 -> BG, P59 -> BK, P69 -> BG, P79 -> BS, P89 -> BN, P99 -> BL
  ), State.EMPTY_HANDS)
  val stateHirateInv = State(WHITE, stateHirate.board, stateHirate.hand)
  val stateEmpty = State(BLACK, Map(), State.EMPTY_HANDS)
  val stateEmptyInv = State(WHITE, Map(), State.EMPTY_HANDS)

  val dataForTest = Seq(
    Game(stateEmpty, Seq(), GameInfo()),
    Game(stateHirate, Seq(
      ExtendedMove(BLACK, P77, P76, PAWN, false, None, false)
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
    Game(stateHirate, Seq(
      ExtendedMove(BLACK, P77, P76, PAWN, false, None, false),
      ExtendedMove(WHITE, P33, P34, PAWN, false, None, false),
      ExtendedMove(BLACK, P88, P22, PBISHOP, true, Some(BISHOP), false),
      ExtendedMove(WHITE, P31, P22, SILVER, false, Some(PBISHOP), false),
      ExtendedMove(BLACK, HAND, P33, BISHOP, false, None, true),
      ExtendedMove(WHITE, P51, P62, KING, false, None, false),
      ExtendedMove(BLACK, P33, P55, PBISHOP, true, None, false)
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
      "+7776FU\n" +
      "-3334FU\n" +
      "+8822UM\n" +
      "-3122GI\n" +
      "+0033KA\n" +
      "-5162OU\n" +
      "+3355UM"
    // TODO: fix me
//      "+7776FU,T50\n" +
//      "-3334FU,T1\n" +
//      "+8822UM,T12\n" +
//      "-3122GI,T100\n" +
//      "+0033KA,T10\n" +
//      "-5162OU,T2\n" +
//      "+3355UM,T3"
  )
  val sfenForTest = Seq(
    "9/9/9/9/9/9/9/9/9 b - 0",
    "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 0 7g7f",
    "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 0 7g7f 3c3d 8h2b+ 3a2b B*3c 5a6b 3c5e+"
  )

  "toCsaString" must "describe some games" in {
    dataForTest.map(_.toCsaString) zip csaForTest foreach { case (a, b) => a must be(b) }
  }
  "parseCsaString" must "work in normal cases" in {
    csaForTest.map(Game.parseCsaString) zip dataForTest.map(Some(_)) foreach { case (a, b) => a must be(b) }
    Game.parseCsaString("+") must be(Some(Game(stateEmpty, Seq(), GameInfo())))
    Game.parseCsaString("-") must be(Some(Game(stateEmptyInv, Seq(), GameInfo())))

    Game.parseCsaString("PI\n-\n-5152OU,T2345\n+5958OU") must be(Some(Game(stateHirateInv, Seq(
      ExtendedMove(WHITE, P51, P52, KING, false, None, false),
      ExtendedMove(BLACK, P59, P58, KING, false, None, false)
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
      "-\n-5152OU,T2345\n+5958OU") must be(Some(Game(stateHirateInv, Seq(
      ExtendedMove(WHITE, P51, P52, KING, false, None, false),
      ExtendedMove(BLACK, P59, P58, KING, false, None, false)
    ), GameInfo(Map('whiteName -> "yyy")))))
    Game.parseCsaString("V2.2\nN+x\nN-y\n$OPENING:AIGAKARI\n+") must be(Some(Game(stateEmpty, Seq(), GameInfo(
      Map('formatVersion -> "2.2", 'blackName -> "x", 'whiteName -> "y", 'opening -> "AIGAKARI")))))
    Game.parseCsaString("V2.2\nN+x\nN-y\n$OPENING:AIGAKARI\n-") must be(Some(Game(State(
      WHITE, stateEmpty.board, stateEmpty.hand), Seq(), GameInfo(
      Map('formatVersion -> "2.2", 'blackName -> "x", 'whiteName -> "y", 'opening -> "AIGAKARI")))))
    Game.parseCsaString("V2.2\n-") must be(Some(Game(stateEmptyInv, Seq(), GameInfo(Map('formatVersion -> "2.2")))))
    Game.parseCsaString("V2.2\n$EVENT:event name\nN-white name\nPI\n-\n-5152OU,T2345\n+5958OU") must be(Some(Game(
      stateHirateInv,
      Seq(
        ExtendedMove(WHITE, P51, P52, KING, false, None, false),
        ExtendedMove(BLACK, P59, P58, KING, false, None, false)
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
//    Game.parseCsaString("N+xxx\nN-yyy\nPI\n+\n+7776FU,TT") must be(None)  // TODO: fix me
    Game.parseCsaString("N+xxx\nN-yyy\nPI\n+\n+7776FU\n+7675FU") must be(None)
    Game.parseCsaString("N+xxx\nN-yyy\nPI\n+\n+7776FU\n-3334KI") must be(None)
    Game.parseCsaString("N+xxx\nN-yyy\nPI\n-\n+7776FU") must be(None)
  }

  "toSfenString" must "describe some games" in {
    dataForTest.map(_.toSfenString) zip sfenForTest foreach { case (a, b) => a must be(b) }
  }
  "parseSfenString" must "create games in normal cases" in {
    sfenForTest.map(Game.parseSfenString) zip dataForTest.map(g => Some(g.copy(gameInfo = GameInfo()))) foreach { case (a, b) => a must be(b) }
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
}
