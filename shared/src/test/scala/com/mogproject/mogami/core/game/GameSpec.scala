package com.mogproject.mogami.core.game

import com.mogproject.mogami._
import com.mogproject.mogami.core.SquareConstant._
import com.mogproject.mogami.core.game.Game.GamePosition
import com.mogproject.mogami.core.game.GameStatus._
import com.mogproject.mogami.core.io.RecordFormatException
import com.mogproject.mogami.core.state.{State, StateCache}
import com.mogproject.mogami.core.state.StateConstant._
import com.mogproject.mogami.core.state.StateCache.Implicits._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, MustMatchers}

class GameSpec extends FlatSpec with MustMatchers with GeneratorDrivenPropertyChecks {

  val stateHirateInv = State(WHITE, HIRATE.board, HIRATE.hand)
  val stateEmpty = State(BLACK, Map(), State.EMPTY_HANDS)
  val stateEmptyInv = State(WHITE, Map(), State.EMPTY_HANDS)


  def createGame(initialState: State,
                 moves: Vector[Move] = Vector.empty,
                 gameInfo: GameInfo = GameInfo(),
                 finalAction: Option[SpecialMove] = None)
                (implicit stateCache: StateCache): Game =
    Game(Branch(stateCache.set(initialState), 0, moves, finalAction), Vector.empty, gameInfo)

  val dataForTest = Seq(
    createGame(stateEmpty, Vector(), GameInfo()),
    createGame(HIRATE, Vector(
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
    createGame(HIRATE, Vector(
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
      |
      |手数----指手----消費時間--
      |""".stripMargin,
    """手合割：平手
      |先手：NAKAHARA
      |後手：YONENAGA
      |
      |手数----指手----消費時間--
      |   1 ７六歩(77)
      |""".stripMargin,
    """手合割：平手
      |先手：B
      |後手：W
      |
      |手数----指手----消費時間--
      |   1 ７六歩(77) (00:50/)
      |   2 ３四歩(33) (00:01/)
      |   3 ２二角成(88) (00:12/)
      |   4 同　銀(31) (01:40/)
      |   5 ３三角打 (00:10/)
      |   6 ６二玉(51) (00:02/)
      |   7 ５五角成(33) (00:03/)
      |""".stripMargin
  )

  "Game#toCsaString" must "describe some games" in {
    dataForTest.map(_.toCsaString) zip csaForTest foreach { case (a, b) => a must be(b) }
  }
  "Game#parseCsaString" must "work in normal cases" in {
    csaForTest.map(Game.parseCsaString) zip dataForTest foreach { case (a, b) => a mustBe b }
    Game.parseCsaString("+") mustBe createGame(stateEmpty, Vector(), GameInfo())
    Game.parseCsaString("-") mustBe createGame(stateEmptyInv, Vector(), GameInfo())

    Game.parseCsaString("PI\n-\n-5152OU,T2345\n+5958OU") mustBe createGame(stateHirateInv, Vector(
      Move(WHITE, Some(P51), P52, KING, false, false, None, None, false, Some(2345)),
      Move(BLACK, Some(P59), P58, KING, false, false, None, None, false)
    ), GameInfo())
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
      "-\n-5152OU\nT2345\n+5958OU") mustBe createGame(stateHirateInv, Vector(
      Move(WHITE, Some(P51), P52, KING, false, false, None, None, false, Some(2345)),
      Move(BLACK, Some(P59), P58, KING, false, false, None, None, false)
    ), GameInfo(Map('whiteName -> "yyy")))
    Game.parseCsaString("V2.2\nN+x\nN-y\n$OPENING:AIGAKARI\n+") mustBe createGame(stateEmpty, Vector(), GameInfo(
      Map('formatVersion -> "2.2", 'blackName -> "x", 'whiteName -> "y", 'opening -> "AIGAKARI")))
    Game.parseCsaString("V2.2\nN+x\nN-y\n$OPENING:AIGAKARI\n-") mustBe createGame(State(
      WHITE, stateEmpty.board, stateEmpty.hand), Vector(), GameInfo(
      Map('formatVersion -> "2.2", 'blackName -> "x", 'whiteName -> "y", 'opening -> "AIGAKARI")))
    Game.parseCsaString("V2.2\n-") mustBe createGame(stateEmptyInv, Vector(), GameInfo(Map('formatVersion -> "2.2")))
    Game.parseCsaString("V2.2\n$EVENT:event name\nN-white name\nPI\n-\n-5152OU,T2345\n+5958OU") mustBe createGame(
      stateHirateInv,
      Vector(
        Move(WHITE, Some(P51), P52, KING, false, false, None, None, false, Some(2345)),
        Move(BLACK, Some(P59), P58, KING, false, false, None, None, false)
      ),
      GameInfo(Map('formatVersion -> "2.2", 'event -> "event name", 'whiteName -> "white name")))
  }
  it must "throw an exception when in error cases" in {
    assertThrows[RecordFormatException](Game.parseCsaString(""))
    assertThrows[RecordFormatException](Game.parseCsaString(" "))
    assertThrows[RecordFormatException](Game.parseCsaString("x" * 1000))
    assertThrows[RecordFormatException](Game.parseCsaString("x\n" * 1000))
    assertThrows[RecordFormatException](Game.parseCsaString("$"))
    assertThrows[RecordFormatException](Game.parseCsaString("V\n"))
    assertThrows[RecordFormatException](Game.parseCsaString("V1\nP+00OU\n+"))
    assertThrows[RecordFormatException](Game.parseCsaString("PI\n+\nN+\n+7776FU"))
  }
  it must "throw an exception when game info is invalid" in {
    assertThrows[RecordFormatException](Game.parseCsaString("$XXX:XXX\nPI\n-\n-5152OU,T2345\n+5958OU"))
    assertThrows[RecordFormatException](Game.parseCsaString("N+xxx\nN=yyy\nPI\n-\n-5152OU,T2345\n+5958OU"))
  }
  it must "throw an exception when initial state is invalid" in {
    assertThrows[RecordFormatException](Game.parseCsaString("N+xxx\nN-yyy\nPI"))
    assertThrows[RecordFormatException](Game.parseCsaString("N+xxx\nN-yyy\nPI\n+7776FU"))
    assertThrows[RecordFormatException](Game.parseCsaString("N+xxx\nN-yyy\nPI\nPI\nPI"))
    assertThrows[RecordFormatException](Game.parseCsaString("N+xxx\nN-yyy\nPI\nP\nP"))
    assertThrows[RecordFormatException](Game.parseCsaString("N+xxx\nN-yyy\nPI\nP+00FU\n-\n-5152OU,T2345\n+5958OU"))
  }
  it must "throw an exception when moves are invalid" in {
    assertThrows[RecordFormatException](Game.parseCsaString("N+xxx\nN-yyy\nPI\n+\n+1234AB"))
    assertThrows[RecordFormatException](Game.parseCsaString("N+xxx\nN-yyy\nPI\n+\n+7776FU,TT"))
    assertThrows[RecordFormatException](Game.parseCsaString("N+xxx\nN-yyy\nPI\n+\n+7776FU\n+7675FU"))
    assertThrows[RecordFormatException](Game.parseCsaString("N+xxx\nN-yyy\nPI\n+\n+7776FU\n-3334KI"))
    assertThrows[RecordFormatException](Game.parseCsaString("N+xxx\nN-yyy\nPI\n-\n+7776FU"))
  }
  it must "restore games" in forAll(GameGen.games, minSuccessful(10)) { g =>
    Game.parseCsaString(g.toCsaString) mustBe g
  }
  it must "ignore comments" in {
    Game.parseCsaString("PI,-\n'comment\n-5152OU,T2345\n'comment\n+5958OU") mustBe createGame(stateHirateInv, Vector(
      Move(WHITE, Some(P51), P52, KING, false, false, None, None, false, Some(2345)),
      Move(BLACK, Some(P59), P58, KING, false, false, None, None, false)
    ), GameInfo())
  }
  it must "parse special moves" in {
    Game.parseCsaString("PI,+,+7776FU,T2,%TORYO,T3") mustBe createGame(HIRATE, Vector(
      Move(BLACK, Some(P77), P76, PAWN, false, false, None, None, false, Some(2))
    ), finalAction = Some(Resign(Some(3))))
    Game.parseCsaString("PI,+,+7776FU,T2,%TIME_UP,T3") mustBe createGame(HIRATE, Vector(
      Move(BLACK, Some(P77), P76, PAWN, false, false, None, None, false, Some(2))
    ), finalAction = Some(TimeUp(Some(3))))
    Game.parseCsaString("PI,+,+7776FU,T2,%CHUDAN") mustBe createGame(HIRATE, Vector(
      Move(BLACK, Some(P77), P76, PAWN, false, false, None, None, false, Some(2))
    ), finalAction = Some(Pause))
  }

  "Game#toSfenString" must "describe some games" in {
    dataForTest.map(_.toSfenString) zip sfenForTest foreach { case (a, b) => a must be(b) }
  }
  "Game#parseSfenString" must "create games in normal cases" in {
    sfenForTest.map(Game.parseSfenString) zip dataForTest.map(g =>
      g.copy(gameInfo = GameInfo(), trunk = g.trunk.copy(moves = g.trunk.moves.map(_.copy(elapsedTime = None))))
    ) foreach { case (a, b) =>
      a mustBe b
    }
  }
  it must "throw an exception in error cases" in {
    assertThrows[RecordFormatException](Game.parseSfenString(""))
    assertThrows[RecordFormatException](Game.parseSfenString(" "))
    assertThrows[RecordFormatException](Game.parseSfenString("x" * 1000))
    assertThrows[RecordFormatException](Game.parseSfenString("x\n" * 1000))
    assertThrows[RecordFormatException](Game.parseSfenString("$"))
    assertThrows[RecordFormatException](Game.parseSfenString("9/9/9/9/9/9/9/9/9 B -"))
    assertThrows[RecordFormatException](Game.parseSfenString("9/9/9/9/9/9/9/9/9 B "))
    assertThrows[RecordFormatException](Game.parseSfenString("9/9/9/9/9/9/9/9/9 b - xxxx"))
  }
  it must "restore games" in forAll(GameGen.games, minSuccessful(10)) { g =>
    val s = g.toSfenString
    Game.parseSfenString(s).toSfenString mustBe s
  }

  "Game#toKifString" must "describe some games" in {
    dataForTest.map(_.toKifString) zip kifForTest foreach { case (a, b) => a must be(b) }
  }

  "Game#parseKifString" must "create games in normal cases" in {
    val s =
      """#KIF version=2.0 encoding=UTF-8
        |開始日時：2017/03/07
        |場所：81Dojo (ver.2016/03/20)
        |持ち時間：15分+60秒
        |手合割：平手
        |先手：black123
        |後手：why
        |手数----指手---------消費時間--
        |   1 ７六歩(77)   ( 0:12/)
        |   2 ８四歩(83)   ( 0:11/)
        |   3 ６八銀(79)   ( 0:3/)
        |   4 ８五歩(84)   ( 0:2/)
        |   5 ７七銀(68)   ( 0:2/)
        |   6 ３四歩(33)   ( 0:2/)
        |   7 ２六歩(27)   ( 0:1/)
        |   8 ４二銀(31)   ( 0:12/)
        |   9 ５八金(49)   ( 0:25/)
        |  10 ８六歩(85)   ( 0:39/)
        |  11 同　歩(87)   ( 0:12/)
        |  12 同　飛(82)   ( 0:3/)
        |  13 ８七歩打   ( 0:2/)
        |  14 同　飛成(86)   ( 0:14/)
        |  15 ６六歩(67)   ( 1:6/)
        |  16 ８四龍(87)   ( 0:23/)
        |  17 ２五歩(26)   ( 0:49/)
        |  18 ３三銀(42)   ( 0:18/)
        |  19 ６七金(58)   ( 0:29/)
        |  20 ８六歩打   ( 0:27/)
        |  21 ２四歩(25)   ( 1:43/)
        |  22 同　銀(33)   ( 0:31/)
        |  23 ５六歩(57)   ( 1:4/)
        |  24 ３二金(41)   ( 0:16/)
        |  25 ７九角(88)   ( 0:13/)
        |  26 ８七歩成(86)   ( 0:13/)
        |  27 ８八銀(77)   ( 0:31/)
        |  28 ８六歩打   ( 0:36/)
        |  29 ７七銀(88)   ( 2:24/)
        |  30 同　と(87)   ( 0:14/)
        |  31 同　桂(89)   ( 0:2/)
        |  32 ８七歩成(86)   ( 0:13/)
        |  33 ８五歩打   ( 0:6/)
        |  34 ４四龍(84)   ( 0:28/)
        |  35 ４八銀(39)   ( 0:49/)
        |  36 ８八歩打   ( 0:40/)
        |  37 ６五桂(77)   ( 1:7/)
        |  38 ６四歩(63)   ( 0:24/)
        |  39 ８四歩(85)   ( 0:8/)
        |  40 ７二金(61)   ( 0:28/)
        |  41 ８三歩成(84)   ( 0:37/)
        |  42 同　金(72)   ( 0:2/)
        |  43 ５三桂成(65)   ( 0:45/)
        |  44 同　龍(44)   ( 0:3/)
        |  45 ２四角(79)   ( 1:7/)
        |  46 同　歩(23)   ( 0:5/)
        |  47 同　飛(28)   ( 0:10/)
        |  48 ２三歩打   ( 0:39/)
        |  49 ２五飛(24)   ( 0:20/)
        |  50 ８九歩成(88)   ( 0:28/)
        |  51 ８五飛(25)   ( 0:51/)
        |  52 ７四歩(73)   ( 0:58/)
        |  53 ５五飛(85)   ( 0:20/)
        |  54 同　角(22)   ( 0:18/)
        |  55 同　歩(56)   ( 0:1/)
        |  56 ７八銀打   ( 0:39/)
        |  57 ５四銀打   ( 0:20/)
        |  58 ６九銀(78)   ( 1:15/)
        |  59 ４九玉(59)   ( 0:23/)
        |  60 ５八金打   ( 0:47/)
        |  61 ３八玉(49)   ( 0:27/)
        |  62 ４九角打   ( 0:7/)
        |  63 ２八玉(38)   ( 0:36/)
        |  64 ２七飛打   ( 0:31/)
        |  65 ３九玉(28)   ( 0:12/)
        |  66 ４八金(58)   ( 0:8/)
        |  67 同　玉(39)   ( 0:12/)
        |  68 ５八銀成(69)   ( 0:5/)
        |  69 投了   ( 0:32/)
        |
    """.stripMargin

    Game.parseKifString(s) mustBe Game.parseCsaString(Seq(
      "N+black123",
      "N-why",
      "P1-KY-KE-GI-KI-OU-KI-GI-KE-KY",
      "P2 * -HI *  *  *  *  * -KA * ",
      "P3-FU-FU-FU-FU-FU-FU-FU-FU-FU",
      "P4 *  *  *  *  *  *  *  *  * ",
      "P5 *  *  *  *  *  *  *  *  * ",
      "P6 *  *  *  *  *  *  *  *  * ",
      "P7+FU+FU+FU+FU+FU+FU+FU+FU+FU",
      "P8 * +KA *  *  *  *  * +HI * ",
      "P9+KY+KE+GI+KI+OU+KI+GI+KE+KY",
      "P+",
      "P-",
      "+",
      "+7776FU,T12",
      "-8384FU,T11",
      "+7968GI,T3",
      "-8485FU,T2",
      "+6877GI,T2",
      "-3334FU,T2",
      "+2726FU,T1",
      "-3142GI,T12",
      "+4958KI,T25",
      "-8586FU,T39",
      "+8786FU,T12",
      "-8286HI,T3",
      "+0087FU,T2",
      "-8687RY,T14",
      "+6766FU,T66",
      "-8784RY,T23",
      "+2625FU,T49",
      "-4233GI,T18",
      "+5867KI,T29",
      "-0086FU,T27",
      "+2524FU,T103",
      "-3324GI,T31",
      "+5756FU,T64",
      "-4132KI,T16",
      "+8879KA,T13",
      "-8687TO,T13",
      "+7788GI,T31",
      "-0086FU,T36",
      "+8877GI,T144",
      "-8777TO,T14",
      "+8977KE,T2",
      "-8687TO,T13",
      "+0085FU,T6",
      "-8444RY,T28",
      "+3948GI,T49",
      "-0088FU,T40",
      "+7765KE,T67",
      "-6364FU,T24",
      "+8584FU,T8",
      "-6172KI,T28",
      "+8483TO,T37",
      "-7283KI,T2",
      "+6553NK,T45",
      "-4453RY,T3",
      "+7924KA,T67",
      "-2324FU,T5",
      "+2824HI,T10",
      "-0023FU,T39",
      "+2425HI,T20",
      "-8889TO,T28",
      "+2585HI,T51",
      "-7374FU,T58",
      "+8555HI,T20",
      "-2255KA,T18",
      "+5655FU,T1",
      "-0078GI,T39",
      "+0054GI,T20",
      "-7869GI,T75",
      "+5949OU,T23",
      "-0058KI,T47",
      "+4938OU,T27",
      "-0049KA,T7",
      "+3828OU,T36",
      "-0027HI,T31",
      "+2839OU,T12",
      "-5848KI,T8",
      "+3948OU,T12",
      "-6958NG,T5",
      "%TORYO,T32"
    ).mkString("\n"))
  }
  it must "restore games" in forAll(GameGen.games, minSuccessful(10)) { g =>
    val ts = g.gameInfo.tags
    val gg = g.copy(gameInfo = GameInfo(Map(
      'blackName -> ts.getOrElse('blackName, ""),
      'whiteName -> ts.getOrElse('whiteName, "")
    )))
    Game.parseKifString(gg.toKifString) mustBe gg
  }

  "Game#status" must "return Playing when playing" in {
    Game().trunk.status mustBe Playing
    Game.parseCsaString("PI\n+\n+7776FU\n-5152OU\n+8833UM").trunk.status mustBe Playing
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
    ).mkString("\n")).trunk.status mustBe Playing
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
    ).mkString("\n")).trunk.status mustBe Mated
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
    ).mkString("\n")).trunk.status mustBe Mated
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
    ).mkString("\n")).trunk.status mustBe Mated

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
    ).mkString("\n")).trunk.status mustBe Mated
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
    ).mkString("\n")).trunk.status mustBe PerpetualCheck
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
    ).mkString("\n")).trunk.status mustBe PerpetualCheck
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
    ).mkString("\n")).trunk.status mustBe Uchifuzume
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
    ).mkString("\n")).trunk.status mustBe Drawn
  }
  it must "tell special moves" in {
    Game.parseCsaString("PI,+,+7776FU,%TORYO").trunk.status mustBe Resigned
    Game.parseCsaString("PI,+,+7776FU,%TIME_UP").trunk.status mustBe TimedUp
    Game.parseCsaString("PI,+,+7775FU,%ILLEGAL_MOVE").trunk.status mustBe IllegallyMoved
  }
  it must "tell resigned even though the state is mated" in {
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
      "-",
      "%TORYO"
    ).mkString("\n")).trunk.status mustBe Resigned
  }

  "Game#getAllMoves" must "get all moves" in {
    Game().getAllMoves(0) mustBe Vector.empty

    val s1 = "lnsgkgsnl_1r5b1_ppppppppp_9_9_9_PPPPPPPPP_1B5R1_LNSGKGSNL.b.-~0.6y20io5t2.~2.7ku1im.~2.7bq1im.r~0..i9i8"
    val g1 = Game.parseUsenString(s1)
    g1.getAllMoves(0) mustBe Vector(
      Move(BLACK, Some(Square(55)), Square(46), PAWN, false, false, None, None, false, None, true),
      Move(WHITE, Some(Square(4)), Square(12), KING, false, false, None, None, false, None, true),
      Move(BLACK, Some(Square(46)), Square(37), PAWN, false, false, None, None, false, None, true)
    )

    g1.getAllMoves(1) mustBe Vector(
      Move(BLACK, Some(Square(55)), Square(46), PAWN, false, false, None, None, false, None, true),
      Move(WHITE, Some(Square(4)), Square(12), KING, false, false, None, None, false, None, true),
      Move(BLACK, Some(Square(60)), Square(51), PAWN, false, false, None, None, false, None, true),
      Move(WHITE, Some(Square(12)), Square(11), KING, false, false, None, None, false, None, true)
    )

    g1.getAllMoves(2) mustBe Vector(
      Move(BLACK, Some(Square(55)), Square(46), PAWN, false, false, None, None, false, None, true),
      Move(WHITE, Some(Square(4)), Square(12), KING, false, false, None, None, false, None, true),
      Move(BLACK, Some(Square(58)), Square(49), PAWN, false, false, None, None, false, None, true),
      Move(WHITE, Some(Square(12)), Square(11), KING, false, false, None, None, false, None, true)
    )

    g1.getAllMoves(3) mustBe Vector.empty
    g1.getAllMoves(4) mustBe Vector.empty
    g1.getAllMoves(-1) mustBe Vector.empty
  }

  "Game#hasComment" must "return if a position has a comment" in {
    Game().hasComment(GamePosition(0, 0)) mustBe false
    Game().hasComment(GamePosition(0, 1)) mustBe false
    Game().hasComment(GamePosition(1, 0)) mustBe false

    val s1 = "lnsgkgsnl_1r5b1_ppppppppp_9_9_9_PPPPPPPPP_1B5R1_LNSGKGSNL.b.-~0.6y20io5t2.~2.7ku1im.~2.7bq1im.r~0..i9i8"
    val g1: Game = Game.parseUsenString(s1)
      .updateBranch(0)(br => Some(br.updateComments(Map(1 -> "mv1", 2 -> "mv2", 3 -> "mv3")))).get
      .updateBranch(1)(br => Some(br.updateComments(Map(2 -> "m2")))).get
      .updateBranch(2)(br => Some(br.updateComments(Map(2 -> "v2", 3 -> "v3")))).get

    (for {
      i <- 0 to 4
      j <- 0 to 4
    } yield g1.hasComment(GamePosition(i, j))) mustBe Seq(
      false, true, true, true, false,
      false, true, true, false, false,
      false, true, true, true, false,
      false, false, false, false, false,
      false, false, false, false, false
    )
  }

  "Game#getFinalAction" must "return final actions" in {
    Game().getFinalAction(0) mustBe None
    Game().getFinalAction(1) mustBe None

    val s1 = "lnsgkgsnl_1r5b1_ppppppppp_9_9_9_PPPPPPPPP_1B5R1_LNSGKGSNL.b.-~0.6y20io5t2.~2.7ku1im.~2.7bq1im.r~0..i9i8"
    val g1: Game = Game.parseUsenString(s1)

    g1.getFinalAction(0) mustBe None
    g1.getFinalAction(1) mustBe None
    g1.getFinalAction(2) mustBe Some(Resign())
    g1.getFinalAction(3) mustBe Some(IllegalMove(Move(BLACK, Some(Square(76)), Square(4), KING, false, false, None, None, false, None, false)))
  }

  "Game#truncated" must "return truncated games" in {
    Game().truncated(GamePosition(0, 0)) mustBe Game()
    Game().truncated(GamePosition(0, 1)) mustBe Game()
    Game().truncated(GamePosition(1, 0)) mustBe Game()

    val s1 = "lnsgkgsnl_1r5b1_ppppppppp_9_9_9_PPPPPPPPP_1B5R1_LNSGKGSNL.b.-~0.6y20io5t2.~2.7ku1im.~2.7bq1im.r~0..i9i8"
    val g1: Game = Game.parseUsenString(s1)
      .updateBranch(0)(br => Some(br.updateComments(Map(1 -> "mv1", 2 -> "mv2", 3 -> "mv3")))).get
      .updateBranch(1)(br => Some(br.updateComments(Map(2 -> "m2")))).get
      .updateBranch(2)(br => Some(br.updateComments(Map(2 -> "v2", 4 -> "v4")))).get

    g1.truncated(GamePosition(0, 3)) mustBe g1
    g1.truncated(GamePosition(0, 2)).trunk.moves.length mustBe 2
    g1.truncated(GamePosition(0, 2)).trunk.comments mustBe Map(1 -> "mv1", 2 -> "mv2")
    g1.truncated(GamePosition(2, 3)).branches(2).comments mustBe Map(2 -> "v2")
    g1.truncated(GamePosition(2, 4)).branches(2).status mustBe GameStatus.Playing
    g1.truncated(GamePosition(0, 2)).branches.length mustBe 3
    g1.truncated(GamePosition(0, 1)).branches.length mustBe 1
    g1.truncated(GamePosition(0, 0)).branches.length mustBe 1
  }

  "Game#getState" must "return state" in {
    Game().getState(GamePosition(0, 0)) mustBe Some(HIRATE)
    Game().getState(GamePosition(0, 1)) mustBe None
    Game().getState(GamePosition(1, 0)) mustBe None

    val s1 = Seq(
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

    val g1: Game = Game.parseUsenString(s1)
    g1.getState(GamePosition(1, 0)) mustBe Some(HIRATE)
    g1.getState(GamePosition(2, 11)).get.toSfenString mustBe "lnsgk1snl/1r4gb1/p1pppp2p/6pR1/1p7/2P6/PP1PPPP1P/1BG6/LNS1KGSNL w Pp"
    g1.getState(GamePosition(2, 12)).get.toSfenString mustBe "lnsgk1s1l/1r4gb1/p1ppppn1p/6pR1/1p7/2P6/PP1PPPP1P/1BG6/LNS1KGSNL b Pp"
    g1.getState(GamePosition(2, 13)).get.toSfenString mustBe "lnsgk1s1l/1r4gb1/p1ppppn1p/6R2/1p7/2P6/PP1PPPP1P/1BG6/LNS1KGSNL w 2Pp"
    g1.getState(GamePosition(2, 14)).get.toSfenString mustBe "lnsgk1s1l/1r4gb1/p1ppppn1p/6R2/9/1pP6/PP1PPPP1P/1BG6/LNS1KGSNL b 2Pp"
    g1.getState(GamePosition(2, 15)).get.toSfenString mustBe "lnsgk1s1l/1r4gb1/p1ppppn1p/6R2/9/1PP6/P2PPPP1P/1BG6/LNS1KGSNL w 3Pp"
    g1.getState(GamePosition(2, 16)).get.toSfenString mustBe "lnsgk1s1l/6gb1/p1ppppn1p/6R2/9/1rP6/P2PPPP1P/1BG6/LNS1KGSNL b 3P2p"
    g1.getState(GamePosition(2, 17)) mustBe None

    val s2 = Seq(
      "lnsgkgsnl_1r5b1_ppppppppp_9_9_9_PPPPPPPPP_1B5R1_LNSGKGSNL.b.-",
      "10.6y236e5t24be9qc0e47ku2jm4o22f281kbek3jm.", // Trunk
      "22.3kk.", // Branch: 1
      "21.05m3jm5ge7pe22w.", // Branch: 2
      "18.8uc1cd9yu.", // Branch: 3
      "13.2jm4o22f281k4be.", // Branch: 4
      "13.2jm4o22f27ku1cx9uw.", // Branch: 5
      "13.2jm4o22f281k0e4bek1a43kk.", // Branch: 6
      "13.0e4.", // Branch: 7
      "10.72m2jm83m.", // Branch: 8
      "10.72m36e83m." // Branch: 9
    ).mkString("~")

    val g2: Game = Game.parseUsenString(s2)
    g2.getState(GamePosition(1, 10)) mustBe Some(HIRATE)
    g2.getState(GamePosition(2, 21)).get.toSfenString mustBe "lnsgk1snl/1r4gb1/p1pppp2p/6pR1/1p7/2P6/PP1PPPP1P/1BG6/LNS1KGSNL w Pp"
    g2.getState(GamePosition(2, 22)).get.toSfenString mustBe "lnsgk1s1l/1r4gb1/p1ppppn1p/6pR1/1p7/2P6/PP1PPPP1P/1BG6/LNS1KGSNL b Pp"
    g2.getState(GamePosition(2, 23)).get.toSfenString mustBe "lnsgk1s1l/1r4gb1/p1ppppn1p/6R2/1p7/2P6/PP1PPPP1P/1BG6/LNS1KGSNL w 2Pp"
    g2.getState(GamePosition(2, 24)).get.toSfenString mustBe "lnsgk1s1l/1r4gb1/p1ppppn1p/6R2/9/1pP6/PP1PPPP1P/1BG6/LNS1KGSNL b 2Pp"
    g2.getState(GamePosition(2, 25)).get.toSfenString mustBe "lnsgk1s1l/1r4gb1/p1ppppn1p/6R2/9/1PP6/P2PPPP1P/1BG6/LNS1KGSNL w 3Pp"
    g2.getState(GamePosition(2, 26)).get.toSfenString mustBe "lnsgk1s1l/6gb1/p1ppppn1p/6R2/9/1rP6/P2PPPP1P/1BG6/LNS1KGSNL b 3P2p"
    g2.getState(GamePosition(2, 27)) mustBe None
  }

  "Game#getForkList" must "return fork list" in {
    val s1 = Seq(
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

    val g1: Game = Game.parseUsenString(s1)

    def f(n: Int) = g1.getForkList(n).map { case (pos, vs) => pos -> vs.map { case (m, b) => m.toCsaString -> b } }

    f(0) mustBe Map(
      0 -> Vector(("+3736FU", 8)),
      3 -> Vector(("-3334FU", 4), ("-4132KI", 7)),
      8 -> Vector(("+8877KA", 3)),
      11 -> Vector(("-2133KE", 2)),
      12 -> Vector(("+2426HI", 1))
    )
    f(1) mustBe Map(
      0 -> Vector(("+3736FU", 8)),
      3 -> Vector(("-3334FU", 4), ("-4132KI", 7)),
      8 -> Vector(("+8877KA", 3)),
      11 -> Vector(("-2133KE", 2)),
      12 -> Vector(("+2434HI", 0))
    )
    f(2) mustBe Map(
      0 -> Vector(("+3736FU", 8)),
      3 -> Vector(("-3334FU", 4), ("-4132KI", 7)),
      8 -> Vector(("+8877KA", 3)),
      11 -> Vector(("-0023FU", 0))
    )
    f(3) mustBe Map(
      0 -> Vector(("+3736FU", 8)),
      3 -> Vector(("-3334FU", 4), ("-4132KI", 7)),
      8 -> Vector(("+2524FU", 0))
    )
    f(4) mustBe Map(
      0 -> Vector(("+3736FU", 8)),
      3 -> Vector(("-8485FU", 0), ("-4132KI", 7)),
      6 -> Vector(("+7776FU", 5)),
      7 -> Vector(("-4132KI", 6))
    )
    f(5) mustBe Map(
      0 -> Vector(("+3736FU", 8)),
      3 -> Vector(("-8485FU", 0), ("-4132KI", 7)),
      6 -> Vector(("+2824HI", 4))
    )
    f(6) mustBe Map(
      0 -> Vector(("+3736FU", 8)),
      3 -> Vector(("-8485FU", 0), ("-4132KI", 7)),
      6 -> Vector(("+7776FU", 5)),
      7 -> Vector(("-8485FU", 4))
    )
    f(7) mustBe Map(
      0 -> Vector(("+3736FU", 8)),
      3 -> Vector(("-8485FU", 0), ("-3334FU", 4))
    )
    f(8) mustBe Map(
      0 -> Vector(("+2726FU", 0)),
      1 -> Vector(("-8384FU", 9))
    )
    f(9) mustBe Map(
      0 -> Vector(("+2726FU", 0)),
      1 -> Vector(("-3334FU", 8))
    )
  }

  "Game#hasFork" must "return if the position has a fork" in {
    val s1 = Seq(
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

    val g1: Game = Game.parseUsenString(s1)

    g1.hasFork(GamePosition(0, 0)) mustBe true
    g1.hasFork(GamePosition(0, 1)) mustBe false
    g1.hasFork(GamePosition(0, 12)) mustBe true
    g1.hasFork(GamePosition(0, 13)) mustBe false
    g1.hasFork(GamePosition(7, 3)) mustBe true
    g1.hasFork(GamePosition(99, 0)) mustBe false
  }

  "Game#getForks" must "return forks at the position" in {
    val s1 = Seq(
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

    val g1: Game = Game.parseUsenString(s1)

    g1.getForks(GamePosition(0, 1)) mustBe Vector.empty
    g1.getForks(GamePosition(0, 0)) mustBe Vector(
      (Move(BLACK, Some(Square(56)), Square(47), PAWN, false, false, None, None, false, None, true), 8)
    )
    g1.getForks(GamePosition(0, 12)) mustBe Vector(
      (Move(BLACK, Some(Square(28)), Square(46), ROOK, false, false, None, None, false, None, true), 1)
    )
    g1.getForks(GamePosition(0, 13)) mustBe Vector.empty
    g1.getForks(GamePosition(7, 3)) mustBe Vector(
      (Move(WHITE, Some(Square(34)), Square(43), PAWN, false, false, None, None, false, None, true), 0),
      (Move(WHITE, Some(Square(20)), Square(29), PAWN, false, false, None, None, false, None, true), 4)
    )
    g1.getForks(GamePosition(99, 0)) mustBe Vector.empty
  }

  "Game#createBranch" must "create a new branch" in {
    val s1 = Seq(
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

    val g1: Game = Game.parseUsenString(s1)

    g1.createBranch(GamePosition(0, 13), Move(WHITE, Some(P23), P24, PAWN, false, false, None, None, false, None, true)) mustBe None
    g1.createBranch(GamePosition(0, 12), Move(BLACK, Some(P24), P26, ROOK, false, false, None, None, false, None, true)) mustBe None
    g1.createBranch(GamePosition(0, 12), Move(BLACK, Some(P24), P27, ROOK, false, false, None, None, false, None, true)) mustBe Some(g1.copy(
      branches = g1.branches :+ Branch(g1.trunk.history(12), 12, Vector(Move(BLACK, Some(P24), P27, ROOK, false, false, None, None, false, None, true)))
    ))
    g1.createBranch(GamePosition(0, 1), Move(WHITE, Some(P13), P14, PAWN, false, false, None, None, false, None, true)) mustBe Some(g1.copy(
      branches = g1.branches :+ Branch(g1.trunk.history(1), 1, Vector(Move(WHITE, Some(P13), P14, PAWN, false, false, None, None, false, None, true)))
    ))
    g1.createBranch(GamePosition(2, 16), Move(WHITE, Some(P82), P85, ROOK, false, false, None, None, false, None, true)) mustBe None
    g1.createBranch(GamePosition(2, 15), Move(WHITE, Some(P82), P85, ROOK, false, false, None, None, false, None, true)) mustBe Some(g1.copy(
      branches = g1.branches :+ Branch(g1.trunk.history(11), 11, Vector(
        Move(WHITE, Some(P21), P33, KNIGHT, false, false, None, None, false, None, true),
        Move(BLACK, Some(P24), P34, ROOK, false, false, None, Some(PAWN), false, None, true),
        Move(WHITE, Some(P85), P86, PAWN, false, false, None, None, false, None, true),
        Move(BLACK, Some(P87), P86, PAWN, false, true, None, Some(PAWN), false, None, true),
        Move(WHITE, Some(P82), P85, ROOK, false, false, None, None, false, None, true)
      )))
    )
    g1.createBranch(GamePosition(2, 8), Move(BLACK, Some(P88), P77, BISHOP, false, false, None, None, false, None, true)) mustBe None
    g1.createBranch(GamePosition(5, 6), Move(BLACK, Some(P28), P24, ROOK, false, true, None, Some(PAWN), false, None, true)) mustBe None
    g1.createBranch(GamePosition(5, 7), Move(WHITE, Some(P84), P85, PAWN, false, false, None, None, false, None, true)) mustBe Some(g1.copy(
      branches = g1.branches :+ Branch(g1.trunk.history(3), 3, Vector(
        Move(WHITE, Some(P33), P34, PAWN, false, false, None, None, false, None, true),
        Move(BLACK, Some(P25), P24, PAWN, false, false, None, None, false, None, true),
        Move(WHITE, Some(P23), P24, PAWN, false, true, None, Some(PAWN), false, None, true),
        Move(BLACK, Some(P77), P76, PAWN, false, false, None, None, false, None, true),
        Move(WHITE, Some(P84), P85, PAWN, false, false, None, None, false, None, true)
      )))
    )
    g1.createBranch(GamePosition(5, 0), Move(BLACK, Some(P37), P36, PAWN, false, false, None, None, false, None, true)) mustBe None
    g1.createBranch(GamePosition(5, 0), Move(BLACK, Some(P47), P46, PAWN, false, false, None, None, false, None, true)) mustBe Some(g1.copy(
      branches = g1.branches :+ Branch(g1.trunk.history(0), 0, Vector(
        Move(BLACK, Some(P47), P46, PAWN, false, false, None, None, false, None, true)
      )))
    )
    g1.createBranch(GamePosition(5, 1), Move(WHITE, Some(P82), P52, ROOK, false, false, None, None, false, None, true)) mustBe Some(g1.copy(
      branches = g1.branches :+ Branch(g1.trunk.history(1), 1, Vector(
        Move(WHITE, Some(P82), P52, ROOK, false, false, None, None, false, None, true)
      )))
    )
    g1.createBranch(GamePosition(5, 3), Move(WHITE, Some(P13), P14, PAWN, false, false, None, None, false, None, true)) mustBe Some(g1.copy(
      branches = g1.branches :+ Branch(g1.trunk.history(3), 3, Vector(
        Move(WHITE, Some(P13), P14, PAWN, false, false, None, None, false, None, true)
      )))
    )
    g1.createBranch(GamePosition(5, 4), Move(BLACK, Some(P17), P16, PAWN, false, false, None, None, false, None, true)) mustBe Some(g1.copy(
      branches = g1.branches :+ Branch(g1.trunk.history(3), 3, Vector(
        Move(WHITE, Some(P33), P34, PAWN, false, false, None, None, false, None, true),
        Move(BLACK, Some(P17), P16, PAWN, false, false, None, None, false, None, true)
      )))
    )
  }

  "Game#deleteBranch" must "delete a branch" in {
    val s1 = Seq(
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

    val g1: Game = Game.parseUsenString(s1)

    g1.deleteBranch(0) mustBe None
    g1.deleteBranch(1) mustBe Some(g1.copy(branches = g1.branches.drop(1)))
    g1.deleteBranch(3) mustBe Some(g1.copy(branches = g1.branches.take(2) ++ g1.branches.drop(3)))
    g1.deleteBranch(9) mustBe Some(g1.copy(branches = g1.branches.take(8)))
    g1.deleteBranch(10) mustBe None
  }
}
