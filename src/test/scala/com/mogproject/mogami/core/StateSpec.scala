package com.mogproject.mogami.core

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, MustMatchers}

import com.mogproject.mogami._
import com.mogproject.mogami.core.SquareConstant._
import com.mogproject.mogami.core.PieceConstant._
import com.mogproject.mogami.core.State.PromotionFlag._

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
    ), State.EMPTY_HANDS ++ Map(BP -> 1, BB -> 1, WP -> 1, WR -> 1)),
    State(BLACK, Map(
      P11 -> WPL, P21 -> WPN, P31 -> WPS, P41 -> WG, P51 -> WK, P61 -> WG, P71 -> WPS, P81 -> WPN, P91 -> WPL,
      P22 -> WPB, P82 -> WPR,
      P13 -> WPP, P23 -> WPP, P33 -> WPP, P43 -> WPP, P53 -> WPP, P63 -> WPP, P73 -> WPP, P83 -> WPP, P93 -> WPP,
      P17 -> BPP, P27 -> BPP, P37 -> BPP, P47 -> BPP, P57 -> BPP, P67 -> BPP, P77 -> BPP, P87 -> BPP, P97 -> BPP,
      P28 -> BPR, P88 -> BPB,
      P19 -> BPL, P29 -> BPN, P39 -> BPS, P49 -> BG, P59 -> BK, P69 -> BG, P79 -> BPS, P89 -> BPN, P99 -> BPL
    ), State.EMPTY_HANDS),
    State(BLACK, Map(P51 -> WK),
      State.EMPTY_HANDS ++ Map(BP -> 18, BL -> 4, BN -> 4, BS -> 4, BG -> 4, BB -> 2, BR -> 2)),
    State(WHITE, Map(P59 -> BK),
      State.EMPTY_HANDS ++ Map(WP -> 18, WL -> 4, WN -> 4, WS -> 4, WG -> 4, WB -> 2, WR -> 2))
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

  "State#toCsaString" must "describe the state" in {
    dataForTest(0).toCsaString mustBe csaForTest(0)
    dataForTest(1).toCsaString mustBe csaForTest(1)
    dataForTest(2).toCsaString mustBe csaForTest(2)
    dataForTest(3).toCsaString mustBe csaForTest(3)
    dataForTest(4).toCsaString mustBe csaForTest(4)
    dataForTest(5).toCsaString mustBe csaForTest(5)
  }
  it must "restore states" in forAll(StateGen.statesWithFullPieces) { st =>
    State.parseCsaString(st.toCsaString) mustBe Some(st)
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
    State.parseSfenString(st.toSfenString) mustBe Some(st)
  }

  "makeMove" must "make next state" in {
    State.HIRATE.makeMove(ExtendedMove(BLACK, P77, P76, PAWN, false, None, false)) mustBe Some(State(WHITE, Map(
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
    ), State.EMPTY_HANDS).makeMove(ExtendedMove(BLACK, P13, P12, PPAWN, true, None, false)) mustBe Some(State(WHITE, Map(
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
    ), State.EMPTY_HANDS).makeMove(ExtendedMove(BLACK, P13, P12, PAWN, false, Some(LANCE), false)) mustBe Some(State(WHITE, Map(
      P21 -> WN, P31 -> WS, P41 -> WG, P51 -> WK, P61 -> WG, P71 -> WS, P81 -> WN, P91 -> WL,
      P22 -> WB, P82 -> WR,
      P14 -> WP, P23 -> WP, P33 -> WP, P43 -> WP, P53 -> WP, P63 -> WP, P73 -> WP, P83 -> WP, P93 -> WP,
      P12 -> BP, P27 -> BP, P37 -> BP, P47 -> BP, P57 -> BP, P67 -> BP, P77 -> BP, P87 -> BP, P97 -> BP,
      P28 -> BR, P88 -> BB,
      P19 -> BL, P29 -> BN, P39 -> BS, P49 -> BG, P59 -> BK, P69 -> BG, P79 -> BS, P89 -> BN, P99 -> BL
    ), State.EMPTY_HANDS.updated(BL, 1)))
    State(BLACK, Map(
      P12 -> WPL, P21 -> WN, P31 -> WS, P41 -> WG, P51 -> WK, P61 -> WG, P71 -> WS, P81 -> WN, P91 -> WL,
      P22 -> WB, P82 -> WR,
      P14 -> WP, P23 -> WP, P33 -> WP, P43 -> WP, P53 -> WP, P63 -> WP, P73 -> WP, P83 -> WP, P93 -> WP,
      P13 -> BP, P27 -> BP, P37 -> BP, P47 -> BP, P57 -> BP, P67 -> BP, P77 -> BP, P87 -> BP, P97 -> BP,
      P28 -> BR, P88 -> BB,
      P29 -> BN, P39 -> BS, P49 -> BG, P59 -> BK, P69 -> BG, P79 -> BS, P89 -> BN, P99 -> BL
    ), State.EMPTY_HANDS.updated(BL, 1)).makeMove(ExtendedMove(BLACK, P13, P12, PAWN, false, Some(PLANCE), false)) mustBe Some(State(WHITE, Map(
      P21 -> WN, P31 -> WS, P41 -> WG, P51 -> WK, P61 -> WG, P71 -> WS, P81 -> WN, P91 -> WL,
      P22 -> WB, P82 -> WR,
      P14 -> WP, P23 -> WP, P33 -> WP, P43 -> WP, P53 -> WP, P63 -> WP, P73 -> WP, P83 -> WP, P93 -> WP,
      P12 -> BP, P27 -> BP, P37 -> BP, P47 -> BP, P57 -> BP, P67 -> BP, P77 -> BP, P87 -> BP, P97 -> BP,
      P28 -> BR, P88 -> BB,
      P29 -> BN, P39 -> BS, P49 -> BG, P59 -> BK, P69 -> BG, P79 -> BS, P89 -> BN, P99 -> BL
    ), State.EMPTY_HANDS.updated(BL, 2)))
    State(BLACK, Map(
      P11 -> WL, P21 -> WN, P31 -> WS, P41 -> WG, P51 -> WK, P61 -> WG, P71 -> WS, P81 -> WN, P91 -> WL,
      P22 -> WB, P82 -> WR,
      P14 -> WP, P23 -> WP, P33 -> WP, P43 -> WP, P53 -> WP, P63 -> WP, P73 -> WP, P83 -> WP, P93 -> WP,
      P27 -> BP, P37 -> BP, P47 -> BP, P57 -> BP, P67 -> BP, P77 -> BP, P87 -> BP, P97 -> BP,
      P28 -> BR, P88 -> BB,
      P19 -> BL, P29 -> BN, P39 -> BS, P49 -> BG, P59 -> BK, P69 -> BG, P79 -> BS, P89 -> BN, P99 -> BL
    ), State.EMPTY_HANDS.updated(BP, 1)).makeMove(ExtendedMove(BLACK, HAND, P12, PAWN, false, None, false)) mustBe Some(State(WHITE, Map(
      P11 -> WL, P21 -> WN, P31 -> WS, P41 -> WG, P51 -> WK, P61 -> WG, P71 -> WS, P81 -> WN, P91 -> WL,
      P22 -> WB, P82 -> WR,
      P14 -> WP, P23 -> WP, P33 -> WP, P43 -> WP, P53 -> WP, P63 -> WP, P73 -> WP, P83 -> WP, P93 -> WP,
      P12 -> BP, P27 -> BP, P37 -> BP, P47 -> BP, P57 -> BP, P67 -> BP, P77 -> BP, P87 -> BP, P97 -> BP,
      P28 -> BR, P88 -> BB,
      P19 -> BL, P29 -> BN, P39 -> BS, P49 -> BG, P59 -> BK, P69 -> BG, P79 -> BS, P89 -> BN, P99 -> BL
    ), State.EMPTY_HANDS))
  }
  // TODO: error cases

  "getPromotionFlag" must "return flags" in {
    State.HIRATE.getPromotionFlag(P77, P76) mustBe Some(CannotPromote)

    // pawn
    State(BLACK, Map(P12 -> BP), State.EMPTY_HANDS).getPromotionFlag(P12, P11) mustBe Some(MustPromote)
    State(BLACK, Map(P13 -> BP), State.EMPTY_HANDS).getPromotionFlag(P13, P12) mustBe Some(CanPromote)
    State(BLACK, Map(P14 -> BP), State.EMPTY_HANDS).getPromotionFlag(P14, P13) mustBe Some(CanPromote)
    State(BLACK, Map(P15 -> BP), State.EMPTY_HANDS).getPromotionFlag(P15, P14) mustBe Some(CannotPromote)
    State(BLACK, Map(P19 -> BP), State.EMPTY_HANDS).getPromotionFlag(P19, P18) mustBe Some(CannotPromote)
    State(BLACK, Map(P12 -> BPP), State.EMPTY_HANDS).getPromotionFlag(P12, P11) mustBe Some(CannotPromote)
    State(BLACK, Map(P13 -> BPP), State.EMPTY_HANDS).getPromotionFlag(P13, P12) mustBe Some(CannotPromote)
    State(WHITE, Map(P11 -> WP), State.EMPTY_HANDS).getPromotionFlag(P11, P12) mustBe Some(CannotPromote)
    State(WHITE, Map(P16 -> WP), State.EMPTY_HANDS).getPromotionFlag(P16, P17) mustBe Some(CanPromote)
    State(WHITE, Map(P17 -> WP), State.EMPTY_HANDS).getPromotionFlag(P17, P18) mustBe Some(CanPromote)
    State(WHITE, Map(P18 -> WP), State.EMPTY_HANDS).getPromotionFlag(P18, P19) mustBe Some(MustPromote)
    State(WHITE, Map(P17 -> WPP), State.EMPTY_HANDS).getPromotionFlag(P17, P18) mustBe Some(CannotPromote)
    State(WHITE, Map(P18 -> WPP), State.EMPTY_HANDS).getPromotionFlag(P18, P19) mustBe Some(CannotPromote)

    // lance
    State(BLACK, Map(P12 -> BL), State.EMPTY_HANDS).getPromotionFlag(P12, P11) mustBe Some(MustPromote)
    State(BLACK, Map(P13 -> BL), State.EMPTY_HANDS).getPromotionFlag(P13, P12) mustBe Some(CanPromote)
    State(BLACK, Map(P13 -> BL), State.EMPTY_HANDS).getPromotionFlag(P13, P11) mustBe Some(MustPromote)
    State(BLACK, Map(P14 -> BL), State.EMPTY_HANDS).getPromotionFlag(P14, P13) mustBe Some(CanPromote)
    State(BLACK, Map(P15 -> BL), State.EMPTY_HANDS).getPromotionFlag(P15, P14) mustBe Some(CannotPromote)
    State(BLACK, Map(P19 -> BL), State.EMPTY_HANDS).getPromotionFlag(P19, P18) mustBe Some(CannotPromote)
    State(BLACK, Map(P12 -> BPL), State.EMPTY_HANDS).getPromotionFlag(P12, P11) mustBe Some(CannotPromote)
    State(BLACK, Map(P13 -> BPL), State.EMPTY_HANDS).getPromotionFlag(P13, P12) mustBe Some(CannotPromote)
    State(WHITE, Map(P11 -> WL), State.EMPTY_HANDS).getPromotionFlag(P11, P12) mustBe Some(CannotPromote)
    State(WHITE, Map(P16 -> WL), State.EMPTY_HANDS).getPromotionFlag(P16, P17) mustBe Some(CanPromote)
    State(WHITE, Map(P17 -> WL), State.EMPTY_HANDS).getPromotionFlag(P17, P18) mustBe Some(CanPromote)
    State(WHITE, Map(P18 -> WL), State.EMPTY_HANDS).getPromotionFlag(P18, P19) mustBe Some(MustPromote)
    State(WHITE, Map(P17 -> WPL), State.EMPTY_HANDS).getPromotionFlag(P17, P18) mustBe Some(CannotPromote)
    State(WHITE, Map(P18 -> WPL), State.EMPTY_HANDS).getPromotionFlag(P18, P19) mustBe Some(CannotPromote)

    // knight
    State(BLACK, Map(P13 -> BN), State.EMPTY_HANDS).getPromotionFlag(P13, P21) mustBe Some(MustPromote)
    State(BLACK, Map(P14 -> BN), State.EMPTY_HANDS).getPromotionFlag(P14, P22) mustBe Some(MustPromote)
    State(BLACK, Map(P15 -> BN), State.EMPTY_HANDS).getPromotionFlag(P15, P23) mustBe Some(CanPromote)
    State(BLACK, Map(P16 -> BN), State.EMPTY_HANDS).getPromotionFlag(P16, P24) mustBe Some(CannotPromote)
    State(BLACK, Map(P19 -> BN), State.EMPTY_HANDS).getPromotionFlag(P19, P27) mustBe Some(CannotPromote)
    State(BLACK, Map(P13 -> BPN), State.EMPTY_HANDS).getPromotionFlag(P13, P12) mustBe Some(CannotPromote)
    State(BLACK, Map(P14 -> BPN), State.EMPTY_HANDS).getPromotionFlag(P14, P13) mustBe Some(CannotPromote)
    State(WHITE, Map(P11 -> WN), State.EMPTY_HANDS).getPromotionFlag(P11, P23) mustBe Some(CannotPromote)
    State(WHITE, Map(P14 -> WN), State.EMPTY_HANDS).getPromotionFlag(P14, P26) mustBe Some(CannotPromote)
    State(WHITE, Map(P15 -> WN), State.EMPTY_HANDS).getPromotionFlag(P15, P27) mustBe Some(CanPromote)
    State(WHITE, Map(P16 -> WN), State.EMPTY_HANDS).getPromotionFlag(P16, P28) mustBe Some(MustPromote)
    State(WHITE, Map(P17 -> WN), State.EMPTY_HANDS).getPromotionFlag(P17, P29) mustBe Some(MustPromote)
    State(WHITE, Map(P17 -> WPN), State.EMPTY_HANDS).getPromotionFlag(P17, P18) mustBe Some(CannotPromote)
    State(WHITE, Map(P18 -> WPN), State.EMPTY_HANDS).getPromotionFlag(P18, P19) mustBe Some(CannotPromote)

    // others
    State(BLACK, Map(P33 -> BS), State.EMPTY_HANDS).getPromotionFlag(P33, P44) mustBe Some(CanPromote)
    State(BLACK, Map(P44 -> BS), State.EMPTY_HANDS).getPromotionFlag(P44, P33) mustBe Some(CanPromote)
    State(BLACK, Map(P77 -> BS), State.EMPTY_HANDS).getPromotionFlag(P77, P66) mustBe Some(CannotPromote)
    State(WHITE, Map(P77 -> WS), State.EMPTY_HANDS).getPromotionFlag(P77, P66) mustBe Some(CanPromote)
    State(WHITE, Map(P66 -> WS), State.EMPTY_HANDS).getPromotionFlag(P66, P77) mustBe Some(CanPromote)

    // from hand
    State(BLACK, Map.empty, State.EMPTY_HANDS.updated(BP, 1)).getPromotionFlag(HAND, P55) mustBe Some(CannotPromote)
    State(WHITE, Map.empty, State.EMPTY_HANDS.updated(WP, 1)).getPromotionFlag(HAND, P55) mustBe Some(CannotPromote)
  }
  it must "return None when from is invalid" in {
    State.HIRATE.getPromotionFlag(P55, P54) mustBe None
    State.HIRATE.getPromotionFlag(P33, P34) mustBe None
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
    )).get

    s1.getAttackBB(BLACK) mustBe BitBoard(Seq(
      "---------",
      "---------",
      "---------",
      "---------",
      "---------",
      "*********",
      "---------",
      "*-*****-*",
      "---------"
    ).mkString)
    s1.getAttackBB(WHITE) mustBe BitBoard(Seq(
      "---------",
      "*-*****-*",
      "---------",
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
    )).get
    val s2: State = State.parseCsaString(Seq(
      "P1 *  *  *  * -OU *  *  *  * ",
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
    )).get
    val s3: State = State.parseCsaString(Seq(
      "P1 *  *  *  * -OU *  *  *  * ",
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
      "-"
    )).get
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
    )).get
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
    )).get

    s1.isChecked mustBe true
    s2.isChecked mustBe true
    s3.isChecked mustBe true
    s4.isChecked mustBe true
  }
  it must "return false whtn the king is not on the board" in {
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
    )).get
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
    )).get
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
    )).get

    s1.isChecked mustBe false
    s2.isChecked mustBe false
    s3.isChecked mustBe false
  }
}