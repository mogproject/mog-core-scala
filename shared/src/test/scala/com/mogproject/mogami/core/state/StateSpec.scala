package com.mogproject.mogami.core.state

import com.mogproject.mogami._
import com.mogproject.mogami.core.PieceConstant._
import com.mogproject.mogami.core.SquareConstant._
import com.mogproject.mogami.core.state.State.PromotionFlag._
import com.mogproject.mogami.util.Implicits._
import com.mogproject.mogami.util.MapUtil
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, MustMatchers}

class StateSpec extends FlatSpec with MustMatchers with GeneratorDrivenPropertyChecks {

  val dataForTest = Seq(
    State.HIRATE,
    State.empty,
    State(WHITE, Map(
      P11 -> WL, P21 -> WN, P22 -> WS, P32 -> WG, P51 -> WK, P61 -> WG, P71 -> WS, P81 -> WN, P91 -> WL,
      P82 -> WR,
      P14 -> WP, P23 -> WP, P34 -> WP, P43 -> WP, P53 -> WP, P63 -> BPB, P73 -> WP, P83 -> WP, P93 -> WP,
      P16 -> BP, P27 -> BP, P37 -> BP, P47 -> BP, P57 -> BP, P75 -> BP, P87 -> BP, P97 -> BP,
      P19 -> BL, P29 -> BN, P39 -> BS, P49 -> BG, P59 -> BK, P58 -> BG, P78 -> BS, P89 -> BN, P99 -> BL
    ), State.EMPTY_HANDS ++ Map(BP -> 1, BB -> 1, WP -> 1, WR -> 1).mapKeys(Hand.apply)),
    State(BLACK, Map(
      P11 -> WPL, P21 -> WPN, P31 -> WPS, P41 -> WG, P51 -> WK, P61 -> WG, P71 -> WPS, P81 -> WPN, P91 -> WPL,
      P22 -> WPB, P82 -> WPR,
      P13 -> WPP, P23 -> WPP, P33 -> WPP, P43 -> WPP, P53 -> WPP, P63 -> WPP, P73 -> WPP, P83 -> WPP, P93 -> WPP,
      P17 -> BPP, P27 -> BPP, P37 -> BPP, P47 -> BPP, P57 -> BPP, P67 -> BPP, P77 -> BPP, P87 -> BPP, P97 -> BPP,
      P28 -> BPR, P88 -> BPB,
      P19 -> BPL, P29 -> BPN, P39 -> BPS, P49 -> BG, P59 -> BK, P69 -> BG, P79 -> BPS, P89 -> BPN, P99 -> BPL
    ), State.EMPTY_HANDS),
    State(BLACK, Map(P51 -> WK),
      State.EMPTY_HANDS ++ Map(BP -> 18, BL -> 4, BN -> 4, BS -> 4, BG -> 4, BB -> 2, BR -> 2).mapKeys(Hand.apply)),
    State(WHITE, Map(P59 -> BK),
      State.EMPTY_HANDS ++ Map(WP -> 18, WL -> 4, WN -> 4, WS -> 4, WG -> 4, WB -> 2, WR -> 2).mapKeys(Hand.apply))
  )

  val csaForTest = Seq(
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
      "+",
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
    "P1-KY-KE-GI-KI-OU *  * -KE-KY\n" +
      "P2 * -HI *  *  *  * -KI-GI * \n" +
      "P3-FU-FU-FU+UM-FU-FU * -FU * \n" +
      "P4 *  *  *  *  *  * -FU * -FU\n" +
      "P5 *  * +FU *  *  *  *  *  * \n" +
      "P6 *  *  *  *  *  *  *  * +FU\n" +
      "P7+FU+FU *  * +FU+FU+FU+FU * \n" +
      "P8 *  * +GI * +KI *  *  *  * \n" +
      "P9+KY+KE *  * +OU+KI+GI+KE+KY\n" +
      "P+00KA00FU\n" +
      "P-00HI00FU\n" +
      "-",
    "P1-NY-NK-NG-KI-OU-KI-NG-NK-NY\n" +
      "P2 * -RY *  *  *  *  * -UM * \n" +
      "P3-TO-TO-TO-TO-TO-TO-TO-TO-TO\n" +
      "P4 *  *  *  *  *  *  *  *  * \n" +
      "P5 *  *  *  *  *  *  *  *  * \n" +
      "P6 *  *  *  *  *  *  *  *  * \n" +
      "P7+TO+TO+TO+TO+TO+TO+TO+TO+TO\n" +
      "P8 * +UM *  *  *  *  * +RY * \n" +
      "P9+NY+NK+NG+KI+OU+KI+NG+NK+NY\n" +
      "P+\n" +
      "P-\n" +
      "+",
    "P1 *  *  *  * -OU *  *  *  * \n" +
      "P2 *  *  *  *  *  *  *  *  * \n" +
      "P3 *  *  *  *  *  *  *  *  * \n" +
      "P4 *  *  *  *  *  *  *  *  * \n" +
      "P5 *  *  *  *  *  *  *  *  * \n" +
      "P6 *  *  *  *  *  *  *  *  * \n" +
      "P7 *  *  *  *  *  *  *  *  * \n" +
      "P8 *  *  *  *  *  *  *  *  * \n" +
      "P9 *  *  *  *  *  *  *  *  * \n" +
      "P+00HI00HI00KA00KA00KI00KI00KI00KI00GI00GI00GI00GI00KE00KE00KE00KE" +
      "00KY00KY00KY00KY00FU00FU00FU00FU00FU00FU00FU00FU00FU00FU00FU00FU" +
      "00FU00FU00FU00FU00FU00FU\n" +
      "P-\n" +
      "+",
    "P1 *  *  *  *  *  *  *  *  * \n" +
      "P2 *  *  *  *  *  *  *  *  * \n" +
      "P3 *  *  *  *  *  *  *  *  * \n" +
      "P4 *  *  *  *  *  *  *  *  * \n" +
      "P5 *  *  *  *  *  *  *  *  * \n" +
      "P6 *  *  *  *  *  *  *  *  * \n" +
      "P7 *  *  *  *  *  *  *  *  * \n" +
      "P8 *  *  *  *  *  *  *  *  * \n" +
      "P9 *  *  *  * +OU *  *  *  * \n" +
      "P+\n" +
      "P-00HI00HI00KA00KA00KI00KI00KI00KI00GI00GI00GI00GI00KE00KE00KE00KE" +
      "00KY00KY00KY00KY00FU00FU00FU00FU00FU00FU00FU00FU00FU00FU00FU00FU" +
      "00FU00FU00FU00FU00FU00FU\n" +
      "-"
  )

  val kifForTest = Seq(
    Seq(
      "後手の持駒：なし",
      "  ９ ８ ７ ６ ５ ４ ３ ２ １",
      "+---------------------------+",
      "|v香v桂v銀v金v玉v金v銀v桂v香|一",
      "| ・v飛 ・ ・ ・ ・ ・v角 ・|二",
      "|v歩v歩v歩v歩v歩v歩v歩v歩v歩|三",
      "| ・ ・ ・ ・ ・ ・ ・ ・ ・|四",
      "| ・ ・ ・ ・ ・ ・ ・ ・ ・|五",
      "| ・ ・ ・ ・ ・ ・ ・ ・ ・|六",
      "| 歩 歩 歩 歩 歩 歩 歩 歩 歩|七",
      "| ・ 角 ・ ・ ・ ・ ・ 飛 ・|八",
      "| 香 桂 銀 金 玉 金 銀 桂 香|九",
      "+---------------------------+",
      "先手の持駒：なし"
    ),
    Seq(
      "後手の持駒：なし",
      "  ９ ８ ７ ６ ５ ４ ３ ２ １",
      "+---------------------------+",
      "| ・ ・ ・ ・ ・ ・ ・ ・ ・|一",
      "| ・ ・ ・ ・ ・ ・ ・ ・ ・|二",
      "| ・ ・ ・ ・ ・ ・ ・ ・ ・|三",
      "| ・ ・ ・ ・ ・ ・ ・ ・ ・|四",
      "| ・ ・ ・ ・ ・ ・ ・ ・ ・|五",
      "| ・ ・ ・ ・ ・ ・ ・ ・ ・|六",
      "| ・ ・ ・ ・ ・ ・ ・ ・ ・|七",
      "| ・ ・ ・ ・ ・ ・ ・ ・ ・|八",
      "| ・ ・ ・ ・ ・ ・ ・ ・ ・|九",
      "+---------------------------+",
      "先手の持駒：なし"
    ),
    Seq(
      "後手の持駒：飛　歩　",
      "  ９ ８ ７ ６ ５ ４ ３ ２ １",
      "+---------------------------+",
      "|v香v桂v銀v金v玉 ・ ・v桂v香|一",
      "| ・v飛 ・ ・ ・ ・v金v銀 ・|二",
      "|v歩v歩v歩 馬v歩v歩 ・v歩 ・|三",
      "| ・ ・ ・ ・ ・ ・v歩 ・v歩|四",
      "| ・ ・ 歩 ・ ・ ・ ・ ・ ・|五",
      "| ・ ・ ・ ・ ・ ・ ・ ・ 歩|六",
      "| 歩 歩 ・ ・ 歩 歩 歩 歩 ・|七",
      "| ・ ・ 銀 ・ 金 ・ ・ ・ ・|八",
      "| 香 桂 ・ ・ 玉 金 銀 桂 香|九",
      "+---------------------------+",
      "先手の持駒：角　歩　",
      "後手番"
    ),
    Seq(
      "後手の持駒：なし",
      "  ９ ８ ７ ６ ５ ４ ３ ２ １",
      "+---------------------------+",
      "|v杏v圭v全v金v玉v金v全v圭v杏|一",
      "| ・v龍 ・ ・ ・ ・ ・v馬 ・|二",
      "|vとvとvとvとvとvとvとvとvと|三",
      "| ・ ・ ・ ・ ・ ・ ・ ・ ・|四",
      "| ・ ・ ・ ・ ・ ・ ・ ・ ・|五",
      "| ・ ・ ・ ・ ・ ・ ・ ・ ・|六",
      "| と と と と と と と と と|七",
      "| ・ 馬 ・ ・ ・ ・ ・ 龍 ・|八",
      "| 杏 圭 全 金 玉 金 全 圭 杏|九",
      "+---------------------------+",
      "先手の持駒：なし"
    ),
    Seq(
      "後手の持駒：なし",
      "  ９ ８ ７ ６ ５ ４ ３ ２ １",
      "+---------------------------+",
      "| ・ ・ ・ ・v玉 ・ ・ ・ ・|一",
      "| ・ ・ ・ ・ ・ ・ ・ ・ ・|二",
      "| ・ ・ ・ ・ ・ ・ ・ ・ ・|三",
      "| ・ ・ ・ ・ ・ ・ ・ ・ ・|四",
      "| ・ ・ ・ ・ ・ ・ ・ ・ ・|五",
      "| ・ ・ ・ ・ ・ ・ ・ ・ ・|六",
      "| ・ ・ ・ ・ ・ ・ ・ ・ ・|七",
      "| ・ ・ ・ ・ ・ ・ ・ ・ ・|八",
      "| ・ ・ ・ ・ ・ ・ ・ ・ ・|九",
      "+---------------------------+",
      "先手の持駒：飛二　角二　金四　銀四　桂四　香四　歩十八　"
    ),
    Seq(
      "後手の持駒：飛二　角二　金四　銀四　桂四　香四　歩十八　",
      "  ９ ８ ７ ６ ５ ４ ３ ２ １",
      "+---------------------------+",
      "| ・ ・ ・ ・ ・ ・ ・ ・ ・|一",
      "| ・ ・ ・ ・ ・ ・ ・ ・ ・|二",
      "| ・ ・ ・ ・ ・ ・ ・ ・ ・|三",
      "| ・ ・ ・ ・ ・ ・ ・ ・ ・|四",
      "| ・ ・ ・ ・ ・ ・ ・ ・ ・|五",
      "| ・ ・ ・ ・ ・ ・ ・ ・ ・|六",
      "| ・ ・ ・ ・ ・ ・ ・ ・ ・|七",
      "| ・ ・ ・ ・ ・ ・ ・ ・ ・|八",
      "| ・ ・ ・ ・ 玉 ・ ・ ・ ・|九",
      "+---------------------------+",
      "先手の持駒：なし",
      "後手番"
    )
  ).map(_.mkString("\n"))

  "State#apply" must "throw an error when the requirements do not meet" in {
    assertThrows[IllegalArgumentException](State(BLACK, Map.empty, State.EMPTY_HANDS ++ Map(Hand(BP) -> 19)))
    assertThrows[IllegalArgumentException](State(BLACK, Map.empty, Map(Hand(BP) -> 1)))
    assertThrows[IllegalArgumentException](State(BLACK, Map(P11 -> BP), State.EMPTY_HANDS))
    assertThrows[IllegalArgumentException](State(BLACK, Map(P11 -> WK, P12 -> BP), State.EMPTY_HANDS))
    assertThrows[IllegalArgumentException](State(BLACK, Map(P56 -> BP, P55 -> BP), State.EMPTY_HANDS)) // nifu
    assertThrows[IllegalArgumentException](State(BLACK, Map(P11 -> WP, P12 -> WP, P13 -> WP), State.EMPTY_HANDS)) // nifu
    assertThrows[IllegalArgumentException](State(BLACK, Map(P11 -> WP, P12 -> WP, P13 -> WP, P14 -> BP, P15 -> BP), State.EMPTY_HANDS)) // nifu
  }

  "State#toCsaString" must "describe the state" in {
    dataForTest(0).toCsaString mustBe csaForTest(0)
    dataForTest(1).toCsaString mustBe csaForTest(1)
    dataForTest(2).toCsaString mustBe csaForTest(2)
    dataForTest(3).toCsaString mustBe csaForTest(3)
    dataForTest(4).toCsaString mustBe csaForTest(4)
    dataForTest(5).toCsaString mustBe csaForTest(5)
  }
  it must "restore states" in forAll(StateGen.statesWithFullPieces) { st =>
    State.parseCsaString(st.toCsaString) mustBe st
  }

  "State#toSfenString" must "describe the state" in {
    dataForTest(0).toSfenString mustBe "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b -"
    dataForTest(1).toSfenString mustBe "9/9/9/9/9/9/9/9/9 b -"
    dataForTest(2).toSfenString mustBe "lnsgk2nl/1r4gs1/ppp+Bpp1p1/6p1p/2P6/8P/PP2PPPP1/2S1G4/LN2KGSNL w BPrp"
    dataForTest(3).toSfenString mustBe "+l+n+sgkg+s+n+l/1+r5+b1/+p+p+p+p+p+p+p+p+p/9/9/9/+P+P+P+P+P+P+P+P+P/1+B5+R1/+L+N+SGKG+S+N+L b -"
    dataForTest(4).toSfenString mustBe "4k4/9/9/9/9/9/9/9/9 b 2R2B4G4S4N4L18P"
    dataForTest(5).toSfenString mustBe "9/9/9/9/9/9/9/9/4K4 w 2r2b4g4s4n4l18p"
  }
  it must "restore states" in forAll(StateGen.statesWithFullPieces) { st =>
    State.parseSfenString(st.toSfenString) mustBe st
  }

  "State#toUsenString" must "describe the state" in {
    dataForTest(0).toUsenString mustBe "lnsgkgsnl_1r5b1_ppppppppp_9_9_9_PPPPPPPPP_1B5R1_LNSGKGSNL.b.-"
    dataForTest(1).toUsenString mustBe "9_9_9_9_9_9_9_9_9.b.-"
    dataForTest(2).toUsenString mustBe "lnsgk2nl_1r4gs1_pppzBpp1p1_6p1p_2P6_8P_PP2PPPP1_2S1G4_LN2KGSNL.w.BPrp"
    dataForTest(3).toUsenString mustBe "zlznzsgkgzsznzl_1zr5zb1_zpzpzpzpzpzpzpzpzp_9_9_9_zPzPzPzPzPzPzPzPzP_1zB5zR1_zLzNzSGKGzSzNzL.b.-"
    dataForTest(4).toUsenString mustBe "4k4_9_9_9_9_9_9_9_9.b.2R2B4G4S4N4L18P"
    dataForTest(5).toUsenString mustBe "9_9_9_9_9_9_9_9_4K4.w.2r2b4g4s4n4l18p"
  }
  it must "restore states" in forAll(StateGen.statesWithFullPieces) { st =>
    State.parseUsenString(st.toUsenString) mustBe st
  }

  "State#toKifString" must "describe the state" in {
    dataForTest(0).toKifString mustBe kifForTest(0)
    dataForTest(1).toKifString mustBe kifForTest(1)
    dataForTest(2).toKifString mustBe kifForTest(2)
    dataForTest(3).toKifString mustBe kifForTest(3)
    dataForTest(4).toKifString mustBe kifForTest(4)
    dataForTest(5).toKifString mustBe kifForTest(5)
  }
  it must "restore states" in forAll(StateGen.statesWithFullPieces) { st =>
    State.parseKifString(st.toKifString) mustBe st
  }

  "State#makeMove" must "make next state" in {
    State.HIRATE.makeMove(Move(BLACK, Some(P77), P76, PAWN, false, false, None, None, false)) mustBe Some(State(WHITE, Map(
      P11 -> WL, P21 -> WN, P31 -> WS, P41 -> WG, P51 -> WK, P61 -> WG, P71 -> WS, P81 -> WN, P91 -> WL,
      P22 -> WB, P82 -> WR,
      P13 -> WP, P23 -> WP, P33 -> WP, P43 -> WP, P53 -> WP, P63 -> WP, P73 -> WP, P83 -> WP, P93 -> WP,
      P17 -> BP, P27 -> BP, P37 -> BP, P47 -> BP, P57 -> BP, P67 -> BP, P76 -> BP, P87 -> BP, P97 -> BP,
      P28 -> BR, P88 -> BB,
      P19 -> BL, P29 -> BN, P39 -> BS, P49 -> BG, P59 -> BK, P69 -> BG, P79 -> BS, P89 -> BN, P99 -> BL
    ), State.EMPTY_HANDS))
    State(BLACK, Map(
      P11 -> WL, P21 -> WN, P31 -> WS, P41 -> WG, P51 -> WK, P61 -> WG, P71 -> WS, P81 -> WN, P91 -> WL,
      P22 -> WB, P82 -> WR,
      P14 -> WP, P23 -> WP, P33 -> WP, P43 -> WP, P53 -> WP, P63 -> WP, P73 -> WP, P83 -> WP, P93 -> WP,
      P13 -> BP, P27 -> BP, P37 -> BP, P47 -> BP, P57 -> BP, P67 -> BP, P77 -> BP, P87 -> BP, P97 -> BP,
      P28 -> BR, P88 -> BB,
      P19 -> BL, P29 -> BN, P39 -> BS, P49 -> BG, P59 -> BK, P69 -> BG, P79 -> BS, P89 -> BN, P99 -> BL
    ), State.EMPTY_HANDS).makeMove(Move(BLACK, Some(P13), P12, PPAWN, true, false, None, None, false)) mustBe Some(State(WHITE, Map(
      P11 -> WL, P21 -> WN, P31 -> WS, P41 -> WG, P51 -> WK, P61 -> WG, P71 -> WS, P81 -> WN, P91 -> WL,
      P22 -> WB, P82 -> WR,
      P14 -> WP, P23 -> WP, P33 -> WP, P43 -> WP, P53 -> WP, P63 -> WP, P73 -> WP, P83 -> WP, P93 -> WP,
      P12 -> BPP, P27 -> BP, P37 -> BP, P47 -> BP, P57 -> BP, P67 -> BP, P77 -> BP, P87 -> BP, P97 -> BP,
      P28 -> BR, P88 -> BB,
      P19 -> BL, P29 -> BN, P39 -> BS, P49 -> BG, P59 -> BK, P69 -> BG, P79 -> BS, P89 -> BN, P99 -> BL
    ), State.EMPTY_HANDS))
    State(BLACK, Map(
      P12 -> WL, P21 -> WN, P31 -> WS, P41 -> WG, P51 -> WK, P61 -> WG, P71 -> WS, P81 -> WN, P91 -> WL,
      P22 -> WB, P82 -> WR,
      P14 -> WP, P23 -> WP, P33 -> WP, P43 -> WP, P53 -> WP, P63 -> WP, P73 -> WP, P83 -> WP, P93 -> WP,
      P13 -> BP, P27 -> BP, P37 -> BP, P47 -> BP, P57 -> BP, P67 -> BP, P77 -> BP, P87 -> BP, P97 -> BP,
      P28 -> BR, P88 -> BB,
      P19 -> BL, P29 -> BN, P39 -> BS, P49 -> BG, P59 -> BK, P69 -> BG, P79 -> BS, P89 -> BN, P99 -> BL
    ), State.EMPTY_HANDS).makeMove(Move(BLACK, Some(P13), P12, PAWN, false, false, None, Some(LANCE), false)) mustBe Some(State(WHITE, Map(
      P21 -> WN, P31 -> WS, P41 -> WG, P51 -> WK, P61 -> WG, P71 -> WS, P81 -> WN, P91 -> WL,
      P22 -> WB, P82 -> WR,
      P14 -> WP, P23 -> WP, P33 -> WP, P43 -> WP, P53 -> WP, P63 -> WP, P73 -> WP, P83 -> WP, P93 -> WP,
      P12 -> BP, P27 -> BP, P37 -> BP, P47 -> BP, P57 -> BP, P67 -> BP, P77 -> BP, P87 -> BP, P97 -> BP,
      P28 -> BR, P88 -> BB,
      P19 -> BL, P29 -> BN, P39 -> BS, P49 -> BG, P59 -> BK, P69 -> BG, P79 -> BS, P89 -> BN, P99 -> BL
    ), State.EMPTY_HANDS.updated(Hand(BL), 1)))
    State(BLACK, Map(
      P12 -> WPL, P21 -> WN, P31 -> WS, P41 -> WG, P51 -> WK, P61 -> WG, P71 -> WS, P81 -> WN, P91 -> WL,
      P22 -> WB, P82 -> WR,
      P14 -> WP, P23 -> WP, P33 -> WP, P43 -> WP, P53 -> WP, P63 -> WP, P73 -> WP, P83 -> WP, P93 -> WP,
      P13 -> BP, P27 -> BP, P37 -> BP, P47 -> BP, P57 -> BP, P67 -> BP, P77 -> BP, P87 -> BP, P97 -> BP,
      P28 -> BR, P88 -> BB,
      P29 -> BN, P39 -> BS, P49 -> BG, P59 -> BK, P69 -> BG, P79 -> BS, P89 -> BN, P99 -> BL
    ), State.EMPTY_HANDS.updated(Hand(BL), 1)).makeMove(Move(BLACK, Some(P13), P12, PAWN, false, false, None, Some(PLANCE), false)) mustBe Some(State(WHITE, Map(
      P21 -> WN, P31 -> WS, P41 -> WG, P51 -> WK, P61 -> WG, P71 -> WS, P81 -> WN, P91 -> WL,
      P22 -> WB, P82 -> WR,
      P14 -> WP, P23 -> WP, P33 -> WP, P43 -> WP, P53 -> WP, P63 -> WP, P73 -> WP, P83 -> WP, P93 -> WP,
      P12 -> BP, P27 -> BP, P37 -> BP, P47 -> BP, P57 -> BP, P67 -> BP, P77 -> BP, P87 -> BP, P97 -> BP,
      P28 -> BR, P88 -> BB,
      P29 -> BN, P39 -> BS, P49 -> BG, P59 -> BK, P69 -> BG, P79 -> BS, P89 -> BN, P99 -> BL
    ), State.EMPTY_HANDS.updated(Hand(BL), 2)))
    State(BLACK, Map(
      P11 -> WL, P21 -> WN, P31 -> WS, P41 -> WG, P51 -> WK, P61 -> WG, P71 -> WS, P81 -> WN, P91 -> WL,
      P22 -> WB, P82 -> WR,
      P14 -> WP, P23 -> WP, P33 -> WP, P43 -> WP, P53 -> WP, P63 -> WP, P73 -> WP, P83 -> WP, P93 -> WP,
      P27 -> BP, P37 -> BP, P47 -> BP, P57 -> BP, P67 -> BP, P77 -> BP, P87 -> BP, P97 -> BP,
      P28 -> BR, P88 -> BB,
      P19 -> BL, P29 -> BN, P39 -> BS, P49 -> BG, P59 -> BK, P69 -> BG, P79 -> BS, P89 -> BN, P99 -> BL
    ), State.EMPTY_HANDS.updated(Hand(BP), 1)).makeMove(Move(BLACK, None, P12, PAWN, false, false, None, None, false)) mustBe Some(State(WHITE, Map(
      P11 -> WL, P21 -> WN, P31 -> WS, P41 -> WG, P51 -> WK, P61 -> WG, P71 -> WS, P81 -> WN, P91 -> WL,
      P22 -> WB, P82 -> WR,
      P14 -> WP, P23 -> WP, P33 -> WP, P43 -> WP, P53 -> WP, P63 -> WP, P73 -> WP, P83 -> WP, P93 -> WP,
      P12 -> BP, P27 -> BP, P37 -> BP, P47 -> BP, P57 -> BP, P67 -> BP, P77 -> BP, P87 -> BP, P97 -> BP,
      P28 -> BR, P88 -> BB,
      P19 -> BL, P29 -> BN, P39 -> BS, P49 -> BG, P59 -> BK, P69 -> BG, P79 -> BS, P89 -> BN, P99 -> BL
    ), State.EMPTY_HANDS))
  }

  "State#getPromotionFlag" must "return flags" in {
    State.HIRATE.getPromotionFlag(Left(P77), P76) mustBe Some(CannotPromote)

    // pawn
    State(BLACK, Map(P12 -> BP), State.EMPTY_HANDS).getPromotionFlag(Left(P12), P11) mustBe Some(MustPromote)
    State(BLACK, Map(P13 -> BP), State.EMPTY_HANDS).getPromotionFlag(Left(P13), P12) mustBe Some(CanPromote)
    State(BLACK, Map(P14 -> BP), State.EMPTY_HANDS).getPromotionFlag(Left(P14), P13) mustBe Some(CanPromote)
    State(BLACK, Map(P15 -> BP), State.EMPTY_HANDS).getPromotionFlag(Left(P15), P14) mustBe Some(CannotPromote)
    State(BLACK, Map(P19 -> BP), State.EMPTY_HANDS).getPromotionFlag(Left(P19), P18) mustBe Some(CannotPromote)
    State(BLACK, Map(P12 -> BPP), State.EMPTY_HANDS).getPromotionFlag(Left(P12), P11) mustBe Some(CannotPromote)
    State(BLACK, Map(P13 -> BPP), State.EMPTY_HANDS).getPromotionFlag(Left(P13), P12) mustBe Some(CannotPromote)
    State(WHITE, Map(P11 -> WP), State.EMPTY_HANDS).getPromotionFlag(Left(P11), P12) mustBe Some(CannotPromote)
    State(WHITE, Map(P16 -> WP), State.EMPTY_HANDS).getPromotionFlag(Left(P16), P17) mustBe Some(CanPromote)
    State(WHITE, Map(P17 -> WP), State.EMPTY_HANDS).getPromotionFlag(Left(P17), P18) mustBe Some(CanPromote)
    State(WHITE, Map(P18 -> WP), State.EMPTY_HANDS).getPromotionFlag(Left(P18), P19) mustBe Some(MustPromote)
    State(WHITE, Map(P17 -> WPP), State.EMPTY_HANDS).getPromotionFlag(Left(P17), P18) mustBe Some(CannotPromote)
    State(WHITE, Map(P18 -> WPP), State.EMPTY_HANDS).getPromotionFlag(Left(P18), P19) mustBe Some(CannotPromote)

    // lance
    State(BLACK, Map(P12 -> BL), State.EMPTY_HANDS).getPromotionFlag(Left(P12), P11) mustBe Some(MustPromote)
    State(BLACK, Map(P13 -> BL), State.EMPTY_HANDS).getPromotionFlag(Left(P13), P12) mustBe Some(CanPromote)
    State(BLACK, Map(P13 -> BL), State.EMPTY_HANDS).getPromotionFlag(Left(P13), P11) mustBe Some(MustPromote)
    State(BLACK, Map(P14 -> BL), State.EMPTY_HANDS).getPromotionFlag(Left(P14), P13) mustBe Some(CanPromote)
    State(BLACK, Map(P15 -> BL), State.EMPTY_HANDS).getPromotionFlag(Left(P15), P14) mustBe Some(CannotPromote)
    State(BLACK, Map(P19 -> BL), State.EMPTY_HANDS).getPromotionFlag(Left(P19), P18) mustBe Some(CannotPromote)
    State(BLACK, Map(P12 -> BPL), State.EMPTY_HANDS).getPromotionFlag(Left(P12), P11) mustBe Some(CannotPromote)
    State(BLACK, Map(P13 -> BPL), State.EMPTY_HANDS).getPromotionFlag(Left(P13), P12) mustBe Some(CannotPromote)
    State(WHITE, Map(P11 -> WL), State.EMPTY_HANDS).getPromotionFlag(Left(P11), P12) mustBe Some(CannotPromote)
    State(WHITE, Map(P16 -> WL), State.EMPTY_HANDS).getPromotionFlag(Left(P16), P17) mustBe Some(CanPromote)
    State(WHITE, Map(P17 -> WL), State.EMPTY_HANDS).getPromotionFlag(Left(P17), P18) mustBe Some(CanPromote)
    State(WHITE, Map(P18 -> WL), State.EMPTY_HANDS).getPromotionFlag(Left(P18), P19) mustBe Some(MustPromote)
    State(WHITE, Map(P17 -> WPL), State.EMPTY_HANDS).getPromotionFlag(Left(P17), P18) mustBe Some(CannotPromote)
    State(WHITE, Map(P18 -> WPL), State.EMPTY_HANDS).getPromotionFlag(Left(P18), P19) mustBe Some(CannotPromote)

    // knight
    State(BLACK, Map(P13 -> BN), State.EMPTY_HANDS).getPromotionFlag(Left(P13), P21) mustBe Some(MustPromote)
    State(BLACK, Map(P14 -> BN), State.EMPTY_HANDS).getPromotionFlag(Left(P14), P22) mustBe Some(MustPromote)
    State(BLACK, Map(P15 -> BN), State.EMPTY_HANDS).getPromotionFlag(Left(P15), P23) mustBe Some(CanPromote)
    State(BLACK, Map(P16 -> BN), State.EMPTY_HANDS).getPromotionFlag(Left(P16), P24) mustBe Some(CannotPromote)
    State(BLACK, Map(P19 -> BN), State.EMPTY_HANDS).getPromotionFlag(Left(P19), P27) mustBe Some(CannotPromote)
    State(BLACK, Map(P13 -> BPN), State.EMPTY_HANDS).getPromotionFlag(Left(P13), P12) mustBe Some(CannotPromote)
    State(BLACK, Map(P14 -> BPN), State.EMPTY_HANDS).getPromotionFlag(Left(P14), P13) mustBe Some(CannotPromote)
    State(WHITE, Map(P11 -> WN), State.EMPTY_HANDS).getPromotionFlag(Left(P11), P23) mustBe Some(CannotPromote)
    State(WHITE, Map(P14 -> WN), State.EMPTY_HANDS).getPromotionFlag(Left(P14), P26) mustBe Some(CannotPromote)
    State(WHITE, Map(P15 -> WN), State.EMPTY_HANDS).getPromotionFlag(Left(P15), P27) mustBe Some(CanPromote)
    State(WHITE, Map(P16 -> WN), State.EMPTY_HANDS).getPromotionFlag(Left(P16), P28) mustBe Some(MustPromote)
    State(WHITE, Map(P17 -> WN), State.EMPTY_HANDS).getPromotionFlag(Left(P17), P29) mustBe Some(MustPromote)
    State(WHITE, Map(P17 -> WPN), State.EMPTY_HANDS).getPromotionFlag(Left(P17), P18) mustBe Some(CannotPromote)
    State(WHITE, Map(P18 -> WPN), State.EMPTY_HANDS).getPromotionFlag(Left(P18), P19) mustBe Some(CannotPromote)

    // others
    State(BLACK, Map(P33 -> BS), State.EMPTY_HANDS).getPromotionFlag(Left(P33), P44) mustBe Some(CanPromote)
    State(BLACK, Map(P44 -> BS), State.EMPTY_HANDS).getPromotionFlag(Left(P44), P33) mustBe Some(CanPromote)
    State(BLACK, Map(P77 -> BS), State.EMPTY_HANDS).getPromotionFlag(Left(P77), P66) mustBe Some(CannotPromote)
    State(WHITE, Map(P77 -> WS), State.EMPTY_HANDS).getPromotionFlag(Left(P77), P66) mustBe Some(CanPromote)
    State(WHITE, Map(P66 -> WS), State.EMPTY_HANDS).getPromotionFlag(Left(P66), P77) mustBe Some(CanPromote)

    // from hand
    State(BLACK, Map.empty, State.EMPTY_HANDS.updated(Hand(BP), 1)).getPromotionFlag(Right(Hand(BP)), P55) mustBe Some(CannotPromote)
    State(WHITE, Map.empty, State.EMPTY_HANDS.updated(Hand(WP), 1)).getPromotionFlag(Right(Hand(WP)), P55) mustBe Some(CannotPromote)
  }
  it must "return None when from is invalid" in {
    State.HIRATE.getPromotionFlag(Left(P55), P54) mustBe None
    State.HIRATE.getPromotionFlag(Left(P33), P34) mustBe None
  }

  "State#getAttackBB" must "return the sum of attack bitboards" in {
    val s1: State = State.parseCsaString(Seq(
      "P1-KY-KE-GI-KI-OU-KI-GI-KE-KY",
      "P2 * -HI *  *  *  *  * -KA * ",
      "P3-FU-FU-FU-FU * -FU-FU-FU-FU",
      "P4 *  *  *  *  *  *  *  *  * ",
      "P5 *  *  *  *  *  *  *  *  * ",
      "P6 *  *  *  *  *  *  *  *  * ",
      "P7+FU+FU+FU+FU+FU+FU+FU+FU+FU",
      "P8 * +KA *  * -FU *  * +HI * ",
      "P9+KY+KE+GI+KI+OU+KI+GI+KE+KY",
      "P+",
      "P-",
      "+"
    ).mkString("\n"))

    s1.getAttackBB(BLACK) mustBe BitBoard(Seq(
      "---------",
      "---------",
      "---------",
      "---------",
      "---------",
      "*********",
      "*-*---***",
      "*********",
      "*-******-"
    ).mkString)
    s1.getAttackBB(WHITE) mustBe BitBoard(Seq(
      "-******-*",
      "*********",
      "***---*-*",
      "****-****",
      "---------",
      "---------",
      "---------",
      "---------",
      "----*----"
    ).mkString)
  }

  "State#isChecked" must "return true when the player's king is checked" in {
    val s1: State = State.parseCsaString(Seq(
      "P1-KY-KE-GI-KI-OU-KI-GI-KE-KY",
      "P2 * -HI *  *  *  *  * -KA * ",
      "P3-FU-FU-FU-FU * -FU-FU-FU-FU",
      "P4 *  *  *  *  *  *  *  *  * ",
      "P5 *  *  *  *  *  *  *  *  * ",
      "P6 *  *  *  *  *  *  *  *  * ",
      "P7+FU+FU+FU+FU+FU+FU+FU+FU+FU",
      "P8 * +KA *  * -FU *  * +HI * ",
      "P9+KY+KE+GI+KI+OU+KI+GI+KE+KY",
      "P+",
      "P-",
      "+"
    ).mkString("\n"))
    val s2: State = State.parseCsaString(Seq(
      "P1 *  *  *  * -KI *  *  *  * ",
      "P2 *  *  * +OU *  *  *  *  * ",
      "P3 *  *  *  *  *  *  *  *  * ",
      "P4 *  *  *  *  *  *  *  *  * ",
      "P5 *  *  *  *  *  *  *  *  * ",
      "P6 *  *  *  *  *  *  *  *  * ",
      "P7 *  *  *  *  *  *  *  *  * ",
      "P8 *  *  *  *  *  *  *  *  * ",
      "P9 *  *  *  *  *  *  *  *  * ",
      "P+",
      "P-",
      "+"
    ).mkString("\n"))
    val s3: State = State.parseCsaString(Seq(
      "P1 *  *  *  * -OU *  *  *  * ",
      "P2 *  *  * +KI *  *  *  *  * ",
      "P3 *  *  *  *  *  *  *  *  * ",
      "P4 *  *  *  *  *  *  *  *  * ",
      "P5 *  *  *  *  *  *  *  *  * ",
      "P6 *  *  *  *  *  *  *  *  * ",
      "P7 *  *  *  *  *  *  *  *  * ",
      "P8 *  *  *  *  *  *  *  *  * ",
      "P9 *  *  *  *  *  *  *  *  * ",
      "P+",
      "P-",
      "-"
    ).mkString("\n"))
    val s4: State = State.parseCsaString(Seq(
      "P1 *  *  *  *  *  *  * -OU * ",
      "P2 *  *  *  *  *  *  *  *  * ",
      "P3 *  *  *  *  *  *  *  *  * ",
      "P4 *  *  *  *  *  *  *  *  * ",
      "P5 *  *  *  *  *  *  *  *  * ",
      "P6 *  *  *  *  *  *  *  *  * ",
      "P7 *  *  *  *  *  *  *  *  * ",
      "P8+KA *  *  *  *  *  *  *  * ",
      "P9 *  *  *  *  *  *  *  *  * ",
      "P+",
      "P-",
      "-"
    ).mkString("\n"))
    val s5: State = State.parseCsaString(Seq(
      "P1 *  *  *  *  *  *  * -OU * ",
      "P2 *  *  *  *  *  *  *  *  * ",
      "P3 *  *  *  *  * +UM *  *  * ",
      "P4 *  *  *  *  *  *  *  *  * ",
      "P5 *  *  *  *  *  *  *  *  * ",
      "P6 *  *  *  *  *  *  *  *  * ",
      "P7 *  *  *  *  *  *  *  *  * ",
      "P8+KA *  *  *  *  *  *  *  * ",
      "P9 *  *  *  *  *  *  * +KY * ",
      "P+00FU",
      "P-",
      "-"
    ).mkString("\n"))

    s1.isChecked mustBe true
    s2.isChecked mustBe true
    s3.isChecked mustBe true
    s4.isChecked mustBe true
  }
  it must "return false when the king is not on the board" in {
    val s1: State = State.parseCsaString(Seq(
      "P1-KY-KE-GI-KI-OU-KI-GI-KE-KY",
      "P2 * -HI *  *  *  *  * -KA * ",
      "P3-FU-FU-FU-FU-FU-FU-FU-FU-FU",
      "P4 *  *  *  *  *  *  *  *  * ",
      "P5 *  *  *  *  *  *  *  *  * ",
      "P6 *  *  *  *  *  *  *  *  * ",
      "P7+FU+FU+FU+FU+FU+FU+FU+FU+FU",
      "P8 * +KA *  *  *  *  * +HI * ",
      "P9+KY+KE+GI+KI * +KI+GI+KE+KY",
      "P+",
      "P-",
      "+"
    ).mkString("\n"))
    val s2: State = State.parseCsaString(Seq(
      "P1-KY-KE-GI-KI * -KI-GI-KE-KY",
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
      "-"
    ).mkString("\n"))
    val s3: State = State.parseCsaString(Seq(
      "P1-KY-KE-GI-KI * -KI-GI-KE-KY",
      "P2 * -HI *  *  *  *  * -KA * ",
      "P3-FU-FU-FU-FU-FU-FU-FU-FU-FU",
      "P4 *  *  *  *  *  *  *  *  * ",
      "P5 *  *  *  *  *  *  *  *  * ",
      "P6 *  *  *  *  *  *  *  *  * ",
      "P7+FU+FU+FU+FU+FU+FU+FU+FU+FU",
      "P8 * +KA *  *  *  *  * +HI * ",
      "P9+KY+KE+GI+KI * +KI+GI+KE+KY",
      "P+",
      "P-",
      "+"
    ).mkString("\n"))

    s1.isChecked mustBe false
    s2.isChecked mustBe false
    s3.isChecked mustBe false
  }

  "State#getAttacker" must "return attackers" in {
    val s1: State = State.parseCsaString(Seq(
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
      "+"
    ).mkString("\n"))
    val s2: State = State.parseCsaString(Seq(
      "P1-KY-KE-GI-KI-OU-KI-GI-KE-KY",
      "P2 * -HI *  *  *  *  * -KA * ",
      "P3-FU-FU-FU-FU-FU-FU-FU-FU-FU",
      "P4 *  *  *  *  *  *  *  *  * ",
      "P5 *  *  *  *  *  *  *  *  * ",
      "P6 *  *  *  *  *  *  *  *  * ",
      "P7+FU+FU+FU+FU+FU+FU+FU+FU+FU",
      "P8 * +KA *  *  *  *  * +HI * ",
      "P9+KY+KE+GI+KI * +KI+GI+KE+KY",
      "P+",
      "P-",
      "+"
    ).mkString("\n"))
    val s3: State = State.parseCsaString(Seq(
      "P1 *  *  *  *  *  *  * -OU * ",
      "P2 *  *  *  *  *  *  *  *  * ",
      "P3 *  *  *  *  *  *  *  *  * ",
      "P4 *  *  *  *  *  *  *  *  * ",
      "P5 *  *  *  *  *  *  *  *  * ",
      "P6 *  *  *  *  *  *  *  *  * ",
      "P7 *  *  *  *  *  *  *  *  * ",
      "P8+KA *  *  *  *  *  *  *  * ",
      "P9 *  *  *  *  *  *  *  *  * ",
      "P+",
      "P-",
      "-"
    ).mkString("\n"))
    val s4: State = State.parseCsaString(Seq(
      "P1 *  *  *  *  *  *  * -OU * ",
      "P2 *  *  *  *  *  *  *  *  * ",
      "P3 *  *  *  *  * +UM *  *  * ",
      "P4 *  *  *  *  *  *  *  *  * ",
      "P5 *  *  *  *  *  *  *  *  * ",
      "P6 *  *  *  *  *  *  *  *  * ",
      "P7 *  *  *  *  *  *  *  *  * ",
      "P8+KA *  *  *  *  *  *  *  * ",
      "P9 *  *  *  *  *  *  * +KY * ",
      "P+00FU",
      "P-",
      "-"
    ).mkString("\n"))
    val s5: State = State.parseCsaString(Seq(
      "P1 *  *  *  *  *  *  * -OU * ",
      "P2 *  *  *  *  *  *  * -FU * ",
      "P3 *  *  *  *  * -GI *  *  * ",
      "P4 *  * -HI *  *  * +OU *  * ",
      "P5 *  *  *  *  *  *  *  *  * ",
      "P6 *  *  *  *  *  *  *  *  * ",
      "P7 *  *  *  *  *  *  *  *  * ",
      "P8+KA *  *  *  *  *  *  *  * ",
      "P9 *  *  *  *  *  *  * +KY * ",
      "P+00FU",
      "P-",
      "+"
    ).mkString("\n"))

    s1.attackers mustBe Set.empty
    s2.attackers mustBe Set.empty
    s3.attackers mustBe Set(P98)
    s4.attackers mustBe Set(P43, P29)
    s5.attackers mustBe Set(P43, P74)
  }

  "State#guards" must "return guard attributes" in {
    val s1: State = State.parseCsaString(Seq(
      "P1 *  *  *  * -KY *  *  *  * ",
      "P2 *  *  *  * -KY *  *  *  * ",
      "P3-KA *  *  * -KY *  *  * -UM",
      "P4 * +FU *  *  *  *  *  *  * ",
      "P5 *  *  *  *  *  * -GI *  * ",
      "P6 *  *  *  *  *  *  *  *  * ",
      "P7 *  *  *  * +OU * +TO-HI * ",
      "P8 *  *  *  * +FU *  *  *  * ",
      "P9 *  *  *  * -RY *  *  *  * ",
      "P+",
      "P-",
      "+"
    ).mkString("\n"))
    s1.guards mustBe Map(
      P53 -> BitBoard(Seq(
        "---------",
        "----*----",
        "----*----",
        "----*----",
        "----*----",
        "----*----",
        "---------",
        "---------",
        "---------"
      ).mkString),
      P35 -> BitBoard(Seq(
        "---------",
        "---------",
        "--------*",
        "-------*-",
        "------*--",
        "-----*---",
        "---------",
        "---------",
        "---------"
      ).mkString),
      P84 -> BitBoard(Seq(
        "---------",
        "---------",
        "*--------",
        "-*-------",
        "--*------",
        "---*-----",
        "---------",
        "---------",
        "---------"
      ).mkString),
      P37 -> BitBoard(Seq(
        "---------",
        "---------",
        "---------",
        "---------",
        "---------",
        "---------",
        "-----***-",
        "---------",
        "---------"
      ).mkString),
      P58 -> BitBoard(Seq(
        "---------",
        "---------",
        "---------",
        "---------",
        "---------",
        "---------",
        "---------",
        "----*----",
        "----*----"
      ).mkString))
  }

  "State#legalMoves" must "return set with proper size" in {
    State.HIRATE.legalMoves(None).size must be(30)
    State.parseCsaString(
      """P1+HI *  *  *  *  *  *  *  *.
        |P2 *  * +OU * +GI * +GI+GI-OU
        |P3 *  *  *  * +KA *  *  *  *.
        |P4 *  *  *  *  *  *  *  *  *.
        |P5 *  *  *  *  *  *  *  *  *.
        |P6 *  *  *  *  *  *  *  *  *.
        |P7 *  *  *  *  *  *  *  *  *.
        |P8 *  *  *  *  *  *  *  *  *.
        |P9 * +KY * +KY * +KY *  *  *.
        |P+00HI00KA00KI00GI00KE00KY00FU
        |P-00KI00KI00KI00KE00KE00KE00FU00FU00FU00FU00FU00FU00FU00FU00FU00FU00FU00FU00FU00FU00FU00FU00FU
        |+""".stripMargin).legalMoves(None).size must be(593)
    State.parseSfenString("ln1g4l/1ks1g4/1pp6/pg1P2pp1/2P2R2p/PP1NP1PP1/1KNg4P/1S+r6/L7L b 2B2SN4P").legalMoves(None).size mustBe 3
  }

  "State#isMated" must "return true when the state is mated" in {
    State.parseCsaString(
      """P1 *  *  *  *  *  *  *  *  *.
        |P2 *  *  *  *  *  *  *  *  *.
        |P3 *  *  *  *  *  *  *  *  *.
        |P4 *  *  *  *  *  *  *  *  *.
        |P5 *  *  *  *  *  *  *  *  *.
        |P6 *  *  *  *  *  *  *  *  *.
        |P7 *  *  *  *  *  *  *  *  *.
        |P8 *  *  *  *  *  *  *  *  *.
        |P9 *  *  *  *  *  *  *  *  *.
        |P+
        |P-
        |+""".stripMargin).isMated mustBe true
    State.parseCsaString(
      """P1 *  *  *  *  *  *  *  *  *.
        |P2 *  *  *  *  *  *  *  *  *.
        |P3 *  *  *  *  *  *  *  *  *.
        |P4 *  *  *  *  *  *  *  *  *.
        |P5 *  *  *  *  *  *  *  *  *.
        |P6 *  *  *  *  *  *  *  *  *.
        |P7 *  *  *  * -FU *  *  *  *.
        |P8 *  *  *  * -KI *  *  *  *.
        |P9 *  *  *  * +OU *  *  *  *.
        |P+00HI00KA
        |P-
        |+""".stripMargin).isMated mustBe true
    State.parseCsaString(
      """P1 *  *  *  *  *  *  *  *  *.
        |P2 *  *  *  *  *  *  *  *  *.
        |P3 *  *  *  *  *  *  *  *  *.
        |P4 *  *  *  *  *  *  *  *  *.
        |P5 *  *  *  *  *  *  *  *  *.
        |P6 *  *  *  *  *  *  *  *  *.
        |P7 *  *  *  *  *  *  *  *  *.
        |P8 *  * +FU+FU+FU+KI *  *  *.
        |P9 * -HI *  * +OU+FU *  *  *.
        |P+00FU00FU
        |P-
        |+""".stripMargin).isMated mustBe true
    State.parseCsaString(
      """P1 *  *  *  *  *  *  *  *  *.
        |P2 *  *  *  *  *  *  *  *  *.
        |P3 *  *  *  *  *  *  *  *  *.
        |P4 *  *  *  *  *  *  *  *  *.
        |P5 *  *  *  *  *  *  *  *  *.
        |P6 *  *  *  *  *  *  *  *  *.
        |P7 *  *  *  *  *  *  *  *  *.
        |P8 *  * +FU+FU+FU+KI *  *  *.
        |P9 * -HI *  * +OU *  *  *  *.
        |P+00FU00FU
        |P-
        |+""".stripMargin).isMated mustBe true
    State.parseCsaString(
      """P1 *  *  *  *  *  *  *  *  *.
        |P2 *  *  *  *  *  *  *  *  *.
        |P3 *  *  *  *  *  *  *  *  *.
        |P4 *  *  *  *  *  *  *  *  *.
        |P5 *  *  *  *  *  *  *  *  *.
        |P6 *  *  *  *  *  *  *  *  *.
        |P7 *  *  *  * -KI *  *  *  *.
        |P8 *  * -KI *  *  * -KI *  *.
        |P9 *  *  *  * +OU *  *  *  *.
        |P+
        |P-
        |+""".stripMargin).isMated mustBe true
    State.parseCsaString(
      """P1 *  *  *  *  *  *  *  *  *.
        |P2 *  *  *  *  *  *  *  *  *.
        |P3 *  *  *  * +HI *  *  *  *.
        |P4 *  *  *  * -KY *  *  *  *.
        |P5 *  *  *  *  *  *  *  *  *.
        |P6 *  *  *  *  *  *  *  *  *.
        |P7 *  * -KA *  *  *  *  *  *.
        |P8 *  * +FU *  *  * -KI *  *.
        |P9 *  *  * +KE+OU *  *  *  *.
        |P+00FU
        |P-
        |+""".stripMargin).isMated mustBe true
    State.parseCsaString(
      """P1 *  *  *  *  * -KY *  *  *.
        |P2 *  *  *  *  *  *  *  *  *.
        |P3 *  *  *  * -KY *  *  *  *.
        |P4 *  *  *  *  *  *  *  *  *.
        |P5 *  *  *  *  *  *  *  *  *.
        |P6 *  * -KA *  *  *  *  *  *.
        |P7 *  *  *  *  *  *  *  *  *.
        |P8 *  *  * +GI+OU *  *  *  *.
        |P9 *  *  * +KI *  *  *  *  *.
        |P+00FU
        |P-
        |+""".stripMargin).isMated mustBe true
    State.parseCsaString(
      """P1 *  *  *  *  *  * -RY * +OU
        |P2 *  *  *  *  *  *  *  * +TO
        |P3 *  *  *  *  *  *  *  * -HI
        |P4 *  *  *  *  *  *  *  *  *.
        |P5 *  *  *  *  *  *  *  *  *.
        |P6 *  *  *  *  *  *  *  *  *.
        |P7 *  *  *  *  *  *  *  *  *.
        |P8 *  *  *  *  *  *  *  *  *.
        |P9 *  *  *  *  *  *  *  *  *.
        |P+00FU
        |P-
        |+""".stripMargin).isMated mustBe true
    State.parseCsaString(
      """P1 *  *  *  *  *  * -GI-KE-OU
        |P2 *  *  *  *  *  * -KY-KA-KY
        |P3 *  *  *  *  *  * -FU+KE-FU
        |P4 *  *  *  *  *  *  *  *  *.
        |P5 *  *  *  *  *  *  *  *  *.
        |P6 *  *  *  *  *  *  *  *  *.
        |P7 *  *  *  *  *  *  *  *  *.
        |P8 *  *  *  *  *  *  *  *  *.
        |P9 *  *  *  *  *  *  *  *  *.
        |P+
        |P-00HI00KA00KI00GI00KE00KY00FU
        |-""".stripMargin).isMated mustBe true
    State.parseCsaString(
      """P1+NG+HI+KI+GI+HI+KA+NY+GI+KA
        |P2+FU+FU+FU+FU+FU+FU+FU+FU+FU
        |P3+OU+KE *  *  *  *  * +KE *.
        |P4+KY+KE *  *  *  *  * +KE *.
        |P5 *  *  *  *  *  *  *  *  *.
        |P6 *  *  *  *  *  *  *  *  *.
        |P7 *  *  *  *  *  *  *  *  *.
        |P8 *  *  *  *  *  *  *  *  *.
        |P9 *  *  *  *  *  *  *  *  *.
        |P+00FU
        |P-
        |+""".stripMargin).isMated mustBe true
  }
  it must "return false when the state is not mated" in {
    State.parseCsaString(
      """P1 *  *  *  *  *  *  *  *  *.
        |P2 *  *  *  *  *  *  *  *  *.
        |P3 *  *  *  *  *  *  *  *  *.
        |P4 *  *  *  *  *  *  *  *  *.
        |P5 *  *  *  *  *  *  *  *  *.
        |P6 *  *  *  *  *  *  *  *  *.
        |P7 *  *  *  *  *  *  *  *  *.
        |P8 *  *  *  *  *  *  *  *  *.
        |P9 *  *  *  *  *  *  *  *  *.
        |P+00FU
        |P-
        |+""".stripMargin).isMated mustBe false
    State.parseCsaString(
      """P1 *  *  *  *  *  *  *  *  *.
        |P2 *  *  *  *  *  *  *  *  *.
        |P3 *  *  *  *  *  *  *  *  *.
        |P4 *  *  *  *  *  *  *  *  *.
        |P5 *  *  *  *  *  *  *  *  *.
        |P6 *  *  *  *  *  *  *  *  *.
        |P7 *  *  *  * -KA *  *  *  *.
        |P8 *  *  *  * -KI *  *  *  *.
        |P9 *  *  *  * +OU *  *  *  *.
        |P+00HI00KA
        |P-
        |+""".stripMargin).isMated mustBe false
    State.parseCsaString(
      """P1 *  *  *  *  *  *  *  *  *.
        |P2 *  *  *  *  *  *  *  *  *.
        |P3 *  *  *  *  *  *  *  *  *.
        |P4 *  *  *  *  *  *  *  *  *.
        |P5 *  *  *  *  *  *  *  *  *.
        |P6 *  *  *  *  *  *  *  *  *.
        |P7 *  *  *  *  *  *  *  *  *.
        |P8 *  * +KY+FU+FU+KI *  *  *.
        |P9 * -HI *  * +OU+FU *  *  *.
        |P+00FU00FU
        |P-
        |+""".stripMargin).isMated mustBe false
    State.parseCsaString(
      """P1 *  *  *  *  *  *  *  *  *.
        |P2 *  *  *  *  *  *  *  *  *.
        |P3 *  *  *  *  *  *  *  *  *.
        |P4 *  *  *  *  *  *  *  *  *.
        |P5 *  *  *  *  *  *  *  *  *.
        |P6 *  *  *  *  *  *  *  *  *.
        |P7 *  *  *  *  *  *  *  *  *.
        |P8 *  * +FU+FU+FU *  *  *  *.
        |P9 * -HI *  * +OU *  *  *  *.
        |P+00FU00FU
        |P-
        |+""".stripMargin).isMated mustBe false
    State.parseCsaString(
      """P1 *  *  *  *  *  *  *  *  *.
        |P2 *  *  *  *  *  *  *  *  *.
        |P3 *  *  *  *  *  *  *  *  *.
        |P4 *  *  *  *  *  *  *  *  *.
        |P5 *  *  *  *  *  *  *  *  *.
        |P6 *  *  *  *  *  *  *  *  *.
        |P7 *  *  *  * -KI *  *  *  *.
        |P8 *  * -KI *  *  * -KI *  *.
        |P9 *  *  *  * +OU *  *  *  *.
        |P+00KA
        |P-
        |+""".stripMargin).isMated mustBe false
    State.parseCsaString(
      """P1 *  *  *  *  *  *  *  *  *.
        |P2 *  *  *  *  *  *  *  *  *.
        |P3 *  *  *  * +HI *  *  *  *.
        |P4 *  *  *  * -KY *  *  *  *.
        |P5 *  *  *  *  *  *  *  *  *.
        |P6 *  *  *  *  *  *  *  *  *.
        |P7 *  *  *  *  *  *  *  *  *.
        |P8 *  * +FU-KA *  * -KI *  *.
        |P9 *  *  * +KE+OU *  *  *  *.
        |P+00FU
        |P-
        |+""".stripMargin).isMated mustBe false
    State.parseCsaString(
      """P1 *  *  *  *  * -KY *  *  *.
        |P2 *  *  *  *  *  *  *  *  *.
        |P3 *  *  *  * -KY *  *  *  *.
        |P4 *  *  *  *  *  *  *  *  *.
        |P5 *  *  *  *  *  *  *  *  *.
        |P6 *  * -KA *  *  *  *  *  *.
        |P7 *  *  *  *  *  *  *  *  *.
        |P8 *  *  * +GI+OU *  *  *  *.
        |P9 *  *  * -KI *  *  *  *  *.
        |P+00FU
        |P-
        |+""".stripMargin).isMated mustBe false
    State.parseCsaString(
      """P1 *  *  *  *  *  * -RY * +OU
        |P2 *  *  *  *  *  *  *  * +TO
        |P3 *  *  *  *  *  *  *  * -UM
        |P4 *  *  *  *  *  *  *  *  *.
        |P5 *  *  *  *  *  *  *  *  *.
        |P6 *  *  *  *  *  *  *  *  *.
        |P7 *  *  *  *  *  *  *  *  *.
        |P8 *  *  *  *  *  *  *  *  *.
        |P9 *  *  *  *  *  *  *  *  *.
        |P+00FU
        |P-
        |+""".stripMargin).isMated mustBe false
    State.parseCsaString(
      """P1 *  *  *  *  *  * -RY * +OU
        |P2 *  *  *  *  *  * -RY *  *.
        |P3 *  *  *  *  *  *  *  * +KE
        |P4 *  *  *  *  *  *  *  *  *.
        |P5 *  *  *  *  *  *  *  *  *.
        |P6 *  *  *  *  *  *  *  *  *.
        |P7 *  *  *  *  *  *  *  *  *.
        |P8 *  *  *  *  *  *  *  *  *.
        |P9 *  *  *  *  *  *  *  *  *.
        |P+00FU
        |P-
        |+""".stripMargin).isMated mustBe false
    State.parseCsaString(
      """P1 *  *  *  *  *  * -GI-KE-OU
        |P2 *  *  *  *  *  * -KI-KA-KY
        |P3 *  *  *  *  *  * -FU+KE-FU
        |P4 *  *  *  *  *  *  *  *  *.
        |P5 *  *  *  *  *  *  *  *  *.
        |P6 *  *  *  *  *  *  *  *  *.
        |P7 *  *  *  *  *  *  *  *  *.
        |P8 *  *  *  *  *  *  *  *  *.
        |P9 *  *  *  *  *  *  *  *  *.
        |P+
        |P-00HI00KA00KI00GI00KE00KY00FU
        |-""".stripMargin).isMated mustBe false
  }

  "State#hasHand" must "return if the in-hand piece exists" in {
    State.HIRATE.hasHand(Hand(BP)) mustBe false
    State.empty.updateHandPiece(BP, 1).get.hasHand(Hand(BP)) mustBe true
    State.empty.updateHandPiece(BP, 2).get.hasHand(Hand(BP)) mustBe true
    State.empty.updateHandPiece(BP, 18).get.hasHand(Hand(BP)) mustBe true
  }

  "State#getNonSuicidalMovesOnBoard" must "return" in {
    State.parseCsaString(Seq(
      "P1 *  *  *  *  *  *  *  *  * ",
      "P2 *  *  *  *  * +HI-KI-OU * ",
      "P3 *  *  *  *  *  *  *  *  * ",
      "P4 *  *  *  *  *  *  *  *  * ",
      "P5 *  *  *  *  *  *  *  *  * ",
      "P6 *  *  *  *  *  *  *  *  * ",
      "P7 *  *  *  *  *  *  *  *  * ",
      "P8 *  *  *  *  *  *  *  *  * ",
      "P9 *  *  *  *  *  *  *  *  * ",
      "P+",
      "P-",
      "-"
    ).mkString("\n")).getNonSuicidalMovesOnBoard mustBe Map(
      Square(3, 2) -> BitBoard("000.010.000.000.000.000.000.000.000"),
      Square(2, 2) -> BitBoard("007.001.007.000.000.000.000.000.000")
    )
    State.parseCsaString(Seq(
      "P1 *  *  *  *  *  *  *  *  * ",
      "P2 *  *  * +HI * +HI-KI-OU * ",
      "P3 *  *  *  *  *  * -GI *  * ",
      "P4 *  *  *  *  * +KA *  *  * ",
      "P5 *  *  *  *  * +KA *  *  * ",
      "P6 *  *  *  *  *  *  *  *  * ",
      "P7 *  *  *  *  *  *  *  *  * ",
      "P8 *  *  *  *  *  *  *  *  * ",
      "P9 *  *  *  *  *  *  *  *  * ",
      "P+",
      "P-",
      "-"
    ).mkString("\n")).getNonSuicidalMovesOnBoard mustBe Map(
      Square(3, 2) -> BitBoard("000.010.000.000.000.000.000.000.000"),
      Square(2, 2) -> BitBoard("007.000.001.000.000.000.000.000.000"),
      Square(3, 3) -> BitBoard("000.000.000.010.000.000.000.000.000")
    )
  }
  "State#getEscapeMoves" must "return" in {
    State.parseCsaString(Seq(
      "P1 *  *  *  *  *  *  *  *  * ",
      "P2 *  *  *  *  *  *  *  *  * ",
      "P3 *  *  *  *  *  *  *  *  * ",
      "P4 *  *  *  *  *  *  *  *  * ",
      "P5 *  *  *  *  *  *  *  *  * ",
      "P6 *  * +OU+KE-KI *  *  *  * ",
      "P7 *  * +GI-RY *  *  *  *  * ",
      "P8 *  *  *  *  *  *  *  *  * ",
      "P9 *  *  *  *  *  *  *  *  * ",
      "P+",
      "P-",
      "+"
    ).mkString("\n")).getEscapeMoves mustBe Map(
      Left(P76) -> BitBoard("000.000.000.000.340.200.200.000.000"),
      Left(P66) -> BitBoard("000.000.000.000.000.000.000.000.000"),
      Left(P77) -> BitBoard("000.000.000.000.000.000.000.000.000")
    )
    State.parseCsaString(Seq(
      "P1 *  *  *  *  *  *  *  *  * ",
      "P2 *  *  *  *  *  *  *  *  * ",
      "P3 *  *  *  *  * -OU *  *  * ",
      "P4 *  *  *  *  *  *  *  *  * ",
      "P5 *  *  *  *  *  *  * +KA * ",
      "P6 *  *  *  *  *  *  *  *  * ",
      "P7 *  *  *  *  *  *  *  *  * ",
      "P8 *  *  *  *  *  *  *  *  * ",
      "P9 *  *  *  *  * +KY+KY *  * ",
      "P+",
      "P-",
      "-"
    ).mkString("\n")).getEscapeMoves mustBe Map(
      Left(P43) -> BitBoard("000.000.020.020.000.000.000.000.000")
    )
  }

  "State#unusedPtypeCount" must "return numbers of unused pieces" in {
    State.MATING_BLACK.unusedPtypeCount mustBe State.capacity.keys.map(_ -> 0).toMap ++ Map(KING -> 1)
    State.MATING_WHITE.unusedPtypeCount mustBe State.capacity.keys.map(_ -> 0).toMap ++ Map(KING -> 1)
    State.HANDICAP_3_PIECE.unusedPtypeCount mustBe State.capacity.keys.map(_ -> 0).toMap ++ Map(ROOK -> 1, BISHOP -> 1, LANCE -> 1)
    State.HANDICAP_THREE_PAWNS.unusedPtypeCount mustBe Map(KING -> 0, ROOK -> 1, BISHOP -> 1, GOLD -> 2, SILVER -> 2, KNIGHT -> 2, LANCE -> 2, PAWN -> 6)
  }

  "State#isUchifuzumePossible" must "return true if Uchifuzume is possible" in {
    val states = Seq(
      Seq(
        "P1 *  *  *  *  *  *  * -FU-OU",
        "P2 *  *  *  *  *  *  *  *  * ",
        "P3 *  *  *  *  *  *  *  * +TO",
        "P4 *  *  *  *  *  *  *  *  * ",
        "P5 *  *  *  *  *  *  *  *  * ",
        "P6 *  *  *  *  *  *  *  *  * ",
        "P7 *  *  *  *  *  *  *  *  * ",
        "P8 *  *  *  *  *  *  *  *  * ",
        "P9 *  *  *  *  *  *  *  *  * ",
        "P+00FU",
        "P-",
        "+"
      ),
      Seq(
        "P1 *  *  *  *  *  * +TO * -OU",
        "P2 *  *  *  *  *  *  *  *  * ",
        "P3 *  *  *  *  *  *  *  * +TO",
        "P4 *  *  *  *  *  *  *  *  * ",
        "P5 *  *  *  *  *  *  *  *  * ",
        "P6 *  *  *  *  *  *  *  *  * ",
        "P7 *  *  *  *  *  *  *  *  * ",
        "P8 *  *  *  *  *  *  *  *  * ",
        "P9 *  *  *  *  *  *  *  *  * ",
        "P+00FU00FU00FU",
        "P-",
        "+"
      ),
      Seq(
        "P1 *  *  *  *  *  *  *  *  * ",
        "P2 *  *  *  *  *  *  *  *  * ",
        "P3 *  *  *  *  *  *  *  *  * ",
        "P4 *  *  *  *  *  *  *  *  * ",
        "P5 *  *  *  *  *  *  *  *  * ",
        "P6 *  *  *  *  *  *  *  *  * ",
        "P7 *  *  *  *  *  * -OU *  * ",
        "P8 *  *  *  *  *  *  *  *  * ",
        "P9 *  *  *  *  * +KE+OU+KY * ",
        "P+",
        "P-00FU",
        "-"
      ),
      Seq(
        "P1 *  *  *  *  *  *  *  *  * ",
        "P2 *  *  *  *  *  *  *  * -KA",
        "P3 *  *  *  *  *  *  *  *  * ",
        "P4 *  *  *  *  *  *  *  *  * ",
        "P5 *  *  *  *  *  *  *  *  * ",
        "P6 *  *  *  *  *  *  *  *  * ",
        "P7 * -OU *  *  *  *  *  *  * ",
        "P8 *  *  *  *  *  *  *  *  * ",
        "P9+OU *  *  *  *  *  *  *  * ",
        "P+",
        "P-00FU",
        "-"
      ),
      Seq(
        "P1 *  *  *  *  *  *  *  *  * ",
        "P2 *  *  *  *  *  *  *  *  * ",
        "P3 *  *  *  * +KA *  *  *  * ",
        "P4 *  *  *  * -FU *  *  *  * ",
        "P5 *  *  * -KE-OU *  *  *  * ",
        "P6 *  *  * -KY *  * +RY *  * ",
        "P7 *  *  *  *  *  *  *  *  * ",
        "P8 *  *  *  *  *  *  *  *  * ",
        "P9 *  *  *  *  *  *  *  *  * ",
        "P+00FU",
        "P-",
        "+"
      ),
        Seq(
        "P1 *  *  *  *  *  *  *  *  * ",
        "P2 *  *  *  *  *  *  *  *  * ",
        "P3 *  *  *  *  *  *  *  *  * ",
        "P4 *  *  *  *  * -KI *  *  * ",
        "P5 *  *  *  * +KA * +KA *  * ",
        "P6 *  *  *  * +HI+OU+HI *  * ",
        "P7 *  *  *  * +FU+FU+FU *  * ",
        "P8 *  *  *  *  * -TO *  *  * ",
        "P9 *  *  *  *  *  *  *  *  * ",
        "P+00FU",
        "P-00FU",
        "-"
      )
    ).map(s => State.parseCsaString(s.mkString("\n")))

    states.map(_.isUchifuzumePossible) mustBe Seq.fill(states.length)(true)
  }
  it must "return false if Uchifuzume is impossible" in {
    val states = Seq(
      Seq(
        "P1 *  *  *  *  *  *  * -FU-OU",
        "P2 *  *  *  *  *  *  *  *  * ",
        "P3 *  *  *  *  *  *  *  * +TO",
        "P4 *  *  *  *  *  *  *  *  * ",
        "P5 *  *  *  *  *  *  *  *  * ",
        "P6 *  *  *  *  *  *  *  *  * ",
        "P7 *  *  *  *  *  *  *  *  * ",
        "P8 *  *  *  *  *  *  *  *  * ",
        "P9 *  *  *  *  *  *  *  *  * ",
        "P+",
        "P-",
        "+"
      ),
      Seq(
        "P1 *  *  *  *  *  *  * -FU-OU",
        "P2 *  *  *  *  *  *  *  * -FU",
        "P3 *  *  *  *  *  *  *  * +TO",
        "P4 *  *  *  *  *  *  *  *  * ",
        "P5 *  *  *  *  *  *  *  *  * ",
        "P6 *  *  *  *  *  *  *  *  * ",
        "P7 *  *  *  *  *  *  *  *  * ",
        "P8 *  *  *  *  *  *  *  *  * ",
        "P9 *  *  *  *  *  *  *  *  * ",
        "P+00FU",
        "P-",
        "+"
      ),
      Seq(
        "P1 *  *  *  *  *  *  * -KI-OU",
        "P2 *  *  *  *  *  *  *  *  * ",
        "P3 *  *  *  *  *  *  *  * +TO",
        "P4 *  *  *  *  *  *  *  *  * ",
        "P5 *  *  *  *  *  *  *  *  * ",
        "P6 *  *  *  *  *  *  *  *  * ",
        "P7 *  *  *  *  *  *  *  *  * ",
        "P8 *  *  *  *  *  *  *  *  * ",
        "P9 *  *  *  *  *  *  *  *  * ",
        "P+00FU",
        "P-",
        "+"
      ),
      Seq(
        "P1 *  *  *  *  *  *  * -FU-OU",
        "P2 *  *  *  *  *  *  * -KY * ",
        "P3 *  *  *  *  *  *  *  * +FU",
        "P4 *  *  *  *  *  *  *  *  * ",
        "P5 *  *  *  *  *  *  *  *  * ",
        "P6 *  *  *  *  *  *  *  *  * ",
        "P7 *  *  *  *  *  *  *  *  * ",
        "P8 *  *  *  *  *  *  *  *  * ",
        "P9 *  *  *  *  *  *  *  *  * ",
        "P+00FU",
        "P-",
        "+"
      ),
      Seq(
        "P1 *  *  *  *  *  *  * -FU-OU",
        "P2 *  *  *  *  *  *  * -KY * ",
        "P3 *  *  *  *  *  *  * +HI * ",
        "P4 *  *  *  *  *  *  *  *  * ",
        "P5 *  *  *  *  *  *  *  *  * ",
        "P6 *  *  *  *  *  *  *  *  * ",
        "P7 *  *  *  *  *  *  *  *  * ",
        "P8 *  *  *  *  *  *  *  *  * ",
        "P9 *  *  *  *  *  *  *  *  * ",
        "P+00FU",
        "P-",
        "+"
      ),
      Seq(
        "P1 *  *  *  *  *  *  * -FU-OU",
        "P2 *  *  *  *  *  *  *  *  * ",
        "P3 *  *  *  *  *  *  *  * +TO",
        "P4 *  *  *  *  *  *  *  *  * ",
        "P5 *  *  *  *  *  *  *  *  * ",
        "P6 *  *  *  *  *  *  *  *  * ",
        "P7 *  *  *  *  *  *  *  *  * ",
        "P8 *  *  *  *  *  *  *  *  * ",
        "P9 *  *  *  *  *  *  *  *  * ",
        "P+00FU",
        "P-",
        "-"
      ),
      Seq(
        "P1 *  *  *  * +OU *  *  *  * ",
        "P2 *  *  * -TO * -TO *  *  * ",
        "P3 *  *  *  * -UM *  *  *  * ",
        "P4 *  *  *  *  *  *  *  *  * ",
        "P5 *  *  *  *  *  *  *  *  * ",
        "P6 *  *  *  *  *  *  *  *  * ",
        "P7 *  *  *  *  *  *  *  *  * ",
        "P8 *  *  *  *  *  *  *  *  * ",
        "P9 *  *  *  *  *  *  *  *  * ",
        "P+00FU",
        "P-00FU",
        "-"
      ),
      Seq(
        "P1 *  *  *  *  *  *  *  *  * ",
        "P2 *  *  *  *  *  *  *  *  * ",
        "P3 *  *  *  *  *  *  *  *  * ",
        "P4 *  *  *  *  * -KI *  *  * ",
        "P5 *  *  *  * +KA * +KA *  * ",
        "P6 *  *  *  * +HI+OU+HI *  * ",
        "P7 *  *  *  * +FU+FU+FU *  * ",
        "P8 *  *  *  *  * -FU *  *  * ",
        "P9 *  *  *  *  *  *  *  *  * ",
        "P+00FU",
        "P-00FU",
        "-"
      ),
      Seq(
        "P1 *  *  *  * +OU *  *  *  * ",
        "P2 *  *  * -TO * -TO *  *  * ",
        "P3 *  *  *  * -UM *  *  *  * ",
        "P4 *  *  *  *  *  *  *  *  * ",
        "P5 *  *  *  *  *  *  *  *  * ",
        "P6 *  *  *  *  *  *  *  *  * ",
        "P7 *  *  * +RY * +RY *  *  * ",
        "P8 *  *  *  *  *  *  *  *  * ",
        "P9 *  *  *  * -OU *  *  *  * ",
        "P+00FU",
        "P-00FU",
        "+"
      ),
      Seq(
        "P1 *  *  *  *  *  *  *  *  * ",
        "P2 *  *  *  *  *  *  *  *  * ",
        "P3 *  *  *  * +KA *  *  *  * ",
        "P4 *  *  *  * -FU *  *  *  * ",
        "P5 *  *  * -KE-OU *  *  *  * ",
        "P6 *  *  * -KY * +HI *  *  * ",
        "P7 *  *  *  *  *  *  *  *  * ",
        "P8 *  *  *  *  *  *  *  *  * ",
        "P9 *  *  *  *  *  *  *  *  * ",
        "P+00FU",
        "P-",
        "+"
      )
    ).map(s => State.parseCsaString(s.mkString("\n")))

    states.map(_.isUchifuzumePossible) mustBe Seq.fill(states.length)(false)
  }

  "State#createMoveFromNextState" must "create Move" in {
    val states = Seq(
      Seq(
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
        "+"
      ),
      Seq(
        "P1-KY-KE-GI-KI-OU-KI-GI-KE-KY",
        "P2 * -HI *  *  *  *  * -KA * ",
        "P3-FU-FU-FU-FU-FU-FU-FU-FU-FU",
        "P4 *  *  *  *  *  *  *  *  * ",
        "P5 *  *  *  *  *  *  *  *  * ",
        "P6 *  * +FU *  *  *  *  *  * ",
        "P7+FU+FU * +FU+FU+FU+FU+FU+FU",
        "P8 * +KA *  *  *  *  * +HI * ",
        "P9+KY+KE+GI+KI+OU+KI+GI+KE+KY",
        "P+",
        "P-",
        "-"
      ),
      Seq(
        "P1-KY-KE-GI-KI-OU-KI-GI-KE-KY",
        "P2 * -HI *  *  *  *  * -KA * ",
        "P3-FU-FU-FU-FU-FU-FU * -FU-FU",
        "P4 *  *  *  *  *  * -FU *  * ",
        "P5 *  *  *  *  *  *  *  *  * ",
        "P6 *  * +FU *  *  *  *  *  * ",
        "P7+FU+FU * +FU+FU+FU+FU+FU+FU",
        "P8 * +KA *  *  *  *  * +HI * ",
        "P9+KY+KE+GI+KI+OU+KI+GI+KE+KY",
        "P+",
        "P-",
        "+"
      ),
      Seq(
        "P1-KY-KE-GI-KI-OU-KI-GI-KE-KY",
        "P2 * -HI *  *  *  *  * +KA * ",
        "P3-FU-FU-FU-FU-FU-FU * -FU-FU",
        "P4 *  *  *  *  *  * -FU *  * ",
        "P5 *  *  *  *  *  *  *  *  * ",
        "P6 *  * +FU *  *  *  *  *  * ",
        "P7+FU+FU * +FU+FU+FU+FU+FU+FU",
        "P8 *  *  *  *  *  *  * +HI * ",
        "P9+KY+KE+GI+KI+OU+KI+GI+KE+KY",
        "P+00KA",
        "P-",
        "-"
      ),
      Seq(
        "P1-KY-KE-GI-KI-OU-KI-GI-KE-KY",
        "P2 *  *  *  *  *  * -HI+KA * ",
        "P3-FU-FU-FU-FU-FU-FU * -FU-FU",
        "P4 *  *  *  *  *  * -FU *  * ",
        "P5 *  *  *  *  *  *  *  *  * ",
        "P6 *  * +FU *  *  *  *  *  * ",
        "P7+FU+FU * +FU+FU+FU+FU+FU+FU",
        "P8 *  *  *  *  *  *  * +HI * ",
        "P9+KY+KE+GI+KI+OU+KI+GI+KE+KY",
        "P+00KA",
        "P-",
        "+"
      ),
      Seq(
        "P1-KY-KE-GI-KI-OU-KI+UM-KE-KY",
        "P2 *  *  *  *  *  * -HI *  * ",
        "P3-FU-FU-FU-FU-FU-FU * -FU-FU",
        "P4 *  *  *  *  *  * -FU *  * ",
        "P5 *  *  *  *  *  *  *  *  * ",
        "P6 *  * +FU *  *  *  *  *  * ",
        "P7+FU+FU * +FU+FU+FU+FU+FU+FU",
        "P8 *  *  *  *  *  *  * +HI * ",
        "P9+KY+KE+GI+KI+OU+KI+GI+KE+KY",
        "P+00KA00GI",
        "P-",
        "-"
      ),
      Seq(
        "P1-KY-KE-GI-KI-OU * -KI-KE-KY",
        "P2 *  *  *  *  *  * -HI *  * ",
        "P3-FU-FU-FU-FU-FU-FU * -FU-FU",
        "P4 *  *  *  *  *  * -FU *  * ",
        "P5 *  *  *  *  *  *  *  *  * ",
        "P6 *  * +FU *  *  *  *  *  * ",
        "P7+FU+FU * +FU+FU+FU+FU+FU+FU",
        "P8 *  *  *  *  *  *  * +HI * ",
        "P9+KY+KE+GI+KI+OU+KI+GI+KE+KY",
        "P+00KA00GI",
        "P-00KA",
        "+"
      ),
      Seq(
        "P1-KY-KE-GI-KI-OU * -KI-KE-KY",
        "P2 *  *  *  *  *  * -HI *  * ",
        "P3-FU-FU-FU-FU-FU-FU+GI-FU-FU",
        "P4 *  *  *  *  *  * -FU *  * ",
        "P5 *  *  *  *  *  *  *  *  * ",
        "P6 *  * +FU *  *  *  *  *  * ",
        "P7+FU+FU * +FU+FU+FU+FU+FU+FU",
        "P8 *  *  *  *  *  *  * +HI * ",
        "P9+KY+KE+GI+KI+OU+KI+GI+KE+KY",
        "P+00KA",
        "P-00KA",
        "-"
      ),
      Seq(
        "P1-KY-KE-GI-KI-OU * -KI-KE-KY",
        "P2 *  *  *  *  *  * -HI *  * ",
        "P3-FU-FU-FU-FU-FU-FU+GI-FU-FU",
        "P4 *  *  *  *  *  * -FU *  * ",
        "P5 *  *  *  *  *  *  *  *  * ",
        "P6 *  * +FU *  *  *  *  *  * ",
        "P7+FU+FU * +FU+FU+FU+FU+FU+FU",
        "P8 * -KA *  *  *  *  * +HI * ",
        "P9+KY+KE+GI+KI+OU+KI+GI+KE+KY",
        "P+00KA",
        "P-",
        "+"
      ),
      Seq(
        "P1-KY-KE-GI-KI-OU * -KI-KE-KY",
        "P2 *  *  *  *  *  * -HI *  * ",
        "P3-FU-FU-FU-FU-FU-FU * -FU-FU",
        "P4 *  *  *  *  * +NG-FU *  * ",
        "P5 *  *  *  *  *  *  *  *  * ",
        "P6 *  * +FU *  *  *  *  *  * ",
        "P7+FU+FU * +FU+FU+FU+FU+FU+FU",
        "P8 * -KA *  *  *  *  * +HI * ",
        "P9+KY+KE+GI+KI+OU+KI+GI+KE+KY",
        "P+00KA",
        "P-",
        "-"
      ),
      Seq(
        "P1-KY-KE-GI-KI-OU * -KI-KE-KY",
        "P2 *  *  *  *  *  * -HI *  * ",
        "P3-FU-FU-FU-FU-FU-FU * -FU-FU",
        "P4 *  *  *  *  * +NG-FU *  * ",
        "P5 *  *  *  *  *  *  *  *  * ",
        "P6 *  * +FU *  *  *  *  *  * ",
        "P7+FU+FU-UM+FU+FU+FU+FU+FU+FU",
        "P8 *  *  *  *  *  *  * +HI * ",
        "P9+KY+KE+GI+KI+OU+KI+GI+KE+KY",
        "P+00KA",
        "P-",
        "+"
      )
    ).map(s => State.parseCsaString(s.mkString("\n")))
    states(0).createMoveFromNextState(states(1), None) mustBe Some(Move(BLACK, Some(P77), P76, PAWN, false, false, None, None, false))
    states(1).createMoveFromNextState(states(2), Some(P76)) mustBe Some(Move(WHITE, Some(P33), P34, PAWN, false, false, None, None, false))
    states(2).createMoveFromNextState(states(3), Some(P34)) mustBe Some(Move(BLACK, Some(P88), P22, BISHOP, false, false, None, Some(BISHOP), false))
    states(3).createMoveFromNextState(states(4), Some(P22)) mustBe Some(Move(WHITE, Some(P82), P32, ROOK, false, false, None, None, false))
    states(4).createMoveFromNextState(states(5), Some(P32)) mustBe Some(Move(BLACK, Some(P22), P31, PBISHOP, true, false, None, Some(SILVER), false))
    states(5).createMoveFromNextState(states(6), Some(P31)) mustBe Some(Move(WHITE, Some(P41), P31, GOLD, false, true, None, Some(PBISHOP), false))
    states(6).createMoveFromNextState(states(7), Some(P31)) mustBe Some(Move(BLACK, None, P33, SILVER, false, false, None, None, false))
    states(7).createMoveFromNextState(states(8), Some(P33)) mustBe Some(Move(WHITE, None, P88, BISHOP, false, false, None, None, false))
    states(8).createMoveFromNextState(states(9), Some(P88)) mustBe Some(Move(BLACK, Some(P33), P44, PSILVER, true, false, None, None, false))
    states(9).createMoveFromNextState(states(10), Some(P44)) mustBe Some(Move(WHITE, Some(P88), P77, PBISHOP, true, false, None, None, true))

    // error cases
    states(0).createMoveFromNextState(states(3), None) mustBe None
  }
}