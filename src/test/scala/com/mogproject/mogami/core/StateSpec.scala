package com.mogproject.mogami.core

import com.mogproject.mogami.core.Player.{BLACK, WHITE}
import com.mogproject.mogami.core.Ptype._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, MustMatchers}
import com.mogproject.mogami.core.SquareConstant._
import com.mogproject.mogami.core.PieceConstant._
import com.mogproject.mogami.core.Square.HAND
import com.mogproject.mogami.core.State.PromotionFlag._

class StateSpec extends FlatSpec with MustMatchers with GeneratorDrivenPropertyChecks {

  "makeMove" must "make next state" in {
    State.HIRATE.makeMove(Move(P77, P76, Some(BLACK), Some(PAWN), None)) must be(Some(State(WHITE, Map(
      P11 -> WL, P21 -> WN, P31 -> WS, P41 -> WG, P51 -> WK, P61 -> WG, P71 -> WS, P81 -> WN, P91 -> WL,
      P22 -> WB, P82 -> WR,
      P13 -> WP, P23 -> WP, P33 -> WP, P43 -> WP, P53 -> WP, P63 -> WP, P73 -> WP, P83 -> WP, P93 -> WP,
      P17 -> BP, P27 -> BP, P37 -> BP, P47 -> BP, P57 -> BP, P67 -> BP, P76 -> BP, P87 -> BP, P97 -> BP,
      P28 -> BR, P88 -> BB,
      P19 -> BL, P29 -> BN, P39 -> BS, P49 -> BG, P59 -> BK, P69 -> BG, P79 -> BS, P89 -> BN, P99 -> BL
    ), State.EMPTY_HANDS)))
    State(BLACK, Map(
      P11 -> WL, P21 -> WN, P31 -> WS, P41 -> WG, P51 -> WK, P61 -> WG, P71 -> WS, P81 -> WN, P91 -> WL,
      P22 -> WB, P82 -> WR,
      P14 -> WP, P23 -> WP, P33 -> WP, P43 -> WP, P53 -> WP, P63 -> WP, P73 -> WP, P83 -> WP, P93 -> WP,
      P13 -> BP, P27 -> BP, P37 -> BP, P47 -> BP, P57 -> BP, P67 -> BP, P77 -> BP, P87 -> BP, P97 -> BP,
      P28 -> BR, P88 -> BB,
      P19 -> BL, P29 -> BN, P39 -> BS, P49 -> BG, P59 -> BK, P69 -> BG, P79 -> BS, P89 -> BN, P99 -> BL
    ), State.EMPTY_HANDS).makeMove(Move(P13, P12, Some(BLACK), Some(PPAWN), None)) must be(Some(State(WHITE, Map(
      P11 -> WL, P21 -> WN, P31 -> WS, P41 -> WG, P51 -> WK, P61 -> WG, P71 -> WS, P81 -> WN, P91 -> WL,
      P22 -> WB, P82 -> WR,
      P14 -> WP, P23 -> WP, P33 -> WP, P43 -> WP, P53 -> WP, P63 -> WP, P73 -> WP, P83 -> WP, P93 -> WP,
      P12 -> BPP, P27 -> BP, P37 -> BP, P47 -> BP, P57 -> BP, P67 -> BP, P77 -> BP, P87 -> BP, P97 -> BP,
      P28 -> BR, P88 -> BB,
      P19 -> BL, P29 -> BN, P39 -> BS, P49 -> BG, P59 -> BK, P69 -> BG, P79 -> BS, P89 -> BN, P99 -> BL
    ), State.EMPTY_HANDS)))
    State(BLACK, Map(
      P12 -> WL, P21 -> WN, P31 -> WS, P41 -> WG, P51 -> WK, P61 -> WG, P71 -> WS, P81 -> WN, P91 -> WL,
      P22 -> WB, P82 -> WR,
      P14 -> WP, P23 -> WP, P33 -> WP, P43 -> WP, P53 -> WP, P63 -> WP, P73 -> WP, P83 -> WP, P93 -> WP,
      P13 -> BP, P27 -> BP, P37 -> BP, P47 -> BP, P57 -> BP, P67 -> BP, P77 -> BP, P87 -> BP, P97 -> BP,
      P28 -> BR, P88 -> BB,
      P19 -> BL, P29 -> BN, P39 -> BS, P49 -> BG, P59 -> BK, P69 -> BG, P79 -> BS, P89 -> BN, P99 -> BL
    ), State.EMPTY_HANDS).makeMove(Move(P13, P12, Some(BLACK), Some(PAWN), None)) must be(Some(State(WHITE, Map(
      P21 -> WN, P31 -> WS, P41 -> WG, P51 -> WK, P61 -> WG, P71 -> WS, P81 -> WN, P91 -> WL,
      P22 -> WB, P82 -> WR,
      P14 -> WP, P23 -> WP, P33 -> WP, P43 -> WP, P53 -> WP, P63 -> WP, P73 -> WP, P83 -> WP, P93 -> WP,
      P12 -> BP, P27 -> BP, P37 -> BP, P47 -> BP, P57 -> BP, P67 -> BP, P77 -> BP, P87 -> BP, P97 -> BP,
      P28 -> BR, P88 -> BB,
      P19 -> BL, P29 -> BN, P39 -> BS, P49 -> BG, P59 -> BK, P69 -> BG, P79 -> BS, P89 -> BN, P99 -> BL
    ), State.EMPTY_HANDS.updated(BL, 1))))
    State(BLACK, Map(
      P12 -> WPL, P21 -> WN, P31 -> WS, P41 -> WG, P51 -> WK, P61 -> WG, P71 -> WS, P81 -> WN, P91 -> WL,
      P22 -> WB, P82 -> WR,
      P14 -> WP, P23 -> WP, P33 -> WP, P43 -> WP, P53 -> WP, P63 -> WP, P73 -> WP, P83 -> WP, P93 -> WP,
      P13 -> BP, P27 -> BP, P37 -> BP, P47 -> BP, P57 -> BP, P67 -> BP, P77 -> BP, P87 -> BP, P97 -> BP,
      P28 -> BR, P88 -> BB,
      P29 -> BN, P39 -> BS, P49 -> BG, P59 -> BK, P69 -> BG, P79 -> BS, P89 -> BN, P99 -> BL
    ), State.EMPTY_HANDS.updated(BL, 1)).makeMove(Move(P13, P12, Some(BLACK), Some(PAWN), None)) must be(Some(State(WHITE, Map(
      P21 -> WN, P31 -> WS, P41 -> WG, P51 -> WK, P61 -> WG, P71 -> WS, P81 -> WN, P91 -> WL,
      P22 -> WB, P82 -> WR,
      P14 -> WP, P23 -> WP, P33 -> WP, P43 -> WP, P53 -> WP, P63 -> WP, P73 -> WP, P83 -> WP, P93 -> WP,
      P12 -> BP, P27 -> BP, P37 -> BP, P47 -> BP, P57 -> BP, P67 -> BP, P77 -> BP, P87 -> BP, P97 -> BP,
      P28 -> BR, P88 -> BB,
      P29 -> BN, P39 -> BS, P49 -> BG, P59 -> BK, P69 -> BG, P79 -> BS, P89 -> BN, P99 -> BL
    ), State.EMPTY_HANDS.updated(BL, 2))))
    State(BLACK, Map(
      P11 -> WL, P21 -> WN, P31 -> WS, P41 -> WG, P51 -> WK, P61 -> WG, P71 -> WS, P81 -> WN, P91 -> WL,
      P22 -> WB, P82 -> WR,
      P14 -> WP, P23 -> WP, P33 -> WP, P43 -> WP, P53 -> WP, P63 -> WP, P73 -> WP, P83 -> WP, P93 -> WP,
      P27 -> BP, P37 -> BP, P47 -> BP, P57 -> BP, P67 -> BP, P77 -> BP, P87 -> BP, P97 -> BP,
      P28 -> BR, P88 -> BB,
      P19 -> BL, P29 -> BN, P39 -> BS, P49 -> BG, P59 -> BK, P69 -> BG, P79 -> BS, P89 -> BN, P99 -> BL
    ), State.EMPTY_HANDS.updated(BP, 1)).makeMove(Move(HAND, P12, Some(BLACK), Some(PAWN), None)) must be(Some(State(WHITE, Map(
      P11 -> WL, P21 -> WN, P31 -> WS, P41 -> WG, P51 -> WK, P61 -> WG, P71 -> WS, P81 -> WN, P91 -> WL,
      P22 -> WB, P82 -> WR,
      P14 -> WP, P23 -> WP, P33 -> WP, P43 -> WP, P53 -> WP, P63 -> WP, P73 -> WP, P83 -> WP, P93 -> WP,
      P12 -> BP, P27 -> BP, P37 -> BP, P47 -> BP, P57 -> BP, P67 -> BP, P77 -> BP, P87 -> BP, P97 -> BP,
      P28 -> BR, P88 -> BB,
      P19 -> BL, P29 -> BN, P39 -> BS, P49 -> BG, P59 -> BK, P69 -> BG, P79 -> BS, P89 -> BN, P99 -> BL
    ), State.EMPTY_HANDS)))
  }
  // TODO: error cases

  "getPromotionFlag" must "return flags" in {
    State.HIRATE.getPromotionFlag(P77, P76) must be(Some(CannotPromote))

    // pawn
    State(BLACK, Map(P12 -> BP), State.EMPTY_HANDS).getPromotionFlag(P12, P11) must be(Some(MustPromote))
    State(BLACK, Map(P13 -> BP), State.EMPTY_HANDS).getPromotionFlag(P13, P12) must be(Some(CanPromote))
    State(BLACK, Map(P14 -> BP), State.EMPTY_HANDS).getPromotionFlag(P14, P13) must be(Some(CanPromote))
    State(BLACK, Map(P15 -> BP), State.EMPTY_HANDS).getPromotionFlag(P15, P14) must be(Some(CannotPromote))
    State(BLACK, Map(P19 -> BP), State.EMPTY_HANDS).getPromotionFlag(P19, P18) must be(Some(CannotPromote))
    State(BLACK, Map(P12 -> BPP), State.EMPTY_HANDS).getPromotionFlag(P12, P11) must be(Some(CannotPromote))
    State(BLACK, Map(P13 -> BPP), State.EMPTY_HANDS).getPromotionFlag(P13, P12) must be(Some(CannotPromote))
    State(WHITE, Map(P11 -> WP), State.EMPTY_HANDS).getPromotionFlag(P11, P12) must be(Some(CannotPromote))
    State(WHITE, Map(P16 -> WP), State.EMPTY_HANDS).getPromotionFlag(P16, P17) must be(Some(CanPromote))
    State(WHITE, Map(P17 -> WP), State.EMPTY_HANDS).getPromotionFlag(P17, P18) must be(Some(CanPromote))
    State(WHITE, Map(P18 -> WP), State.EMPTY_HANDS).getPromotionFlag(P18, P19) must be(Some(MustPromote))
    State(WHITE, Map(P17 -> WPP), State.EMPTY_HANDS).getPromotionFlag(P17, P18) must be(Some(CannotPromote))
    State(WHITE, Map(P18 -> WPP), State.EMPTY_HANDS).getPromotionFlag(P18, P19) must be(Some(CannotPromote))

    // lance
    State(BLACK, Map(P12 -> BL), State.EMPTY_HANDS).getPromotionFlag(P12, P11) must be(Some(MustPromote))
    State(BLACK, Map(P13 -> BL), State.EMPTY_HANDS).getPromotionFlag(P13, P12) must be(Some(CanPromote))
    State(BLACK, Map(P13 -> BL), State.EMPTY_HANDS).getPromotionFlag(P13, P11) must be(Some(MustPromote))
    State(BLACK, Map(P14 -> BL), State.EMPTY_HANDS).getPromotionFlag(P14, P13) must be(Some(CanPromote))
    State(BLACK, Map(P15 -> BL), State.EMPTY_HANDS).getPromotionFlag(P15, P14) must be(Some(CannotPromote))
    State(BLACK, Map(P19 -> BL), State.EMPTY_HANDS).getPromotionFlag(P19, P18) must be(Some(CannotPromote))
    State(BLACK, Map(P12 -> BPL), State.EMPTY_HANDS).getPromotionFlag(P12, P11) must be(Some(CannotPromote))
    State(BLACK, Map(P13 -> BPL), State.EMPTY_HANDS).getPromotionFlag(P13, P12) must be(Some(CannotPromote))
    State(WHITE, Map(P11 -> WL), State.EMPTY_HANDS).getPromotionFlag(P11, P12) must be(Some(CannotPromote))
    State(WHITE, Map(P16 -> WL), State.EMPTY_HANDS).getPromotionFlag(P16, P17) must be(Some(CanPromote))
    State(WHITE, Map(P17 -> WL), State.EMPTY_HANDS).getPromotionFlag(P17, P18) must be(Some(CanPromote))
    State(WHITE, Map(P18 -> WL), State.EMPTY_HANDS).getPromotionFlag(P18, P19) must be(Some(MustPromote))
    State(WHITE, Map(P17 -> WPL), State.EMPTY_HANDS).getPromotionFlag(P17, P18) must be(Some(CannotPromote))
    State(WHITE, Map(P18 -> WPL), State.EMPTY_HANDS).getPromotionFlag(P18, P19) must be(Some(CannotPromote))

    // knight
    State(BLACK, Map(P13 -> BN), State.EMPTY_HANDS).getPromotionFlag(P13, P21) must be(Some(MustPromote))
    State(BLACK, Map(P14 -> BN), State.EMPTY_HANDS).getPromotionFlag(P14, P22) must be(Some(MustPromote))
    State(BLACK, Map(P15 -> BN), State.EMPTY_HANDS).getPromotionFlag(P15, P23) must be(Some(CanPromote))
    State(BLACK, Map(P16 -> BN), State.EMPTY_HANDS).getPromotionFlag(P16, P24) must be(Some(CannotPromote))
    State(BLACK, Map(P19 -> BN), State.EMPTY_HANDS).getPromotionFlag(P19, P27) must be(Some(CannotPromote))
    State(BLACK, Map(P13 -> BPN), State.EMPTY_HANDS).getPromotionFlag(P13, P12) must be(Some(CannotPromote))
    State(BLACK, Map(P14 -> BPN), State.EMPTY_HANDS).getPromotionFlag(P14, P13) must be(Some(CannotPromote))
    State(WHITE, Map(P11 -> WN), State.EMPTY_HANDS).getPromotionFlag(P11, P23) must be(Some(CannotPromote))
    State(WHITE, Map(P14 -> WN), State.EMPTY_HANDS).getPromotionFlag(P14, P26) must be(Some(CannotPromote))
    State(WHITE, Map(P15 -> WN), State.EMPTY_HANDS).getPromotionFlag(P15, P27) must be(Some(CanPromote))
    State(WHITE, Map(P16 -> WN), State.EMPTY_HANDS).getPromotionFlag(P16, P28) must be(Some(MustPromote))
    State(WHITE, Map(P17 -> WN), State.EMPTY_HANDS).getPromotionFlag(P17, P29) must be(Some(MustPromote))
    State(WHITE, Map(P17 -> WPN), State.EMPTY_HANDS).getPromotionFlag(P17, P18) must be(Some(CannotPromote))
    State(WHITE, Map(P18 -> WPN), State.EMPTY_HANDS).getPromotionFlag(P18, P19) must be(Some(CannotPromote))

    // others
    State(BLACK, Map(P33 -> BS), State.EMPTY_HANDS).getPromotionFlag(P33, P44) must be(Some(CanPromote))
    State(BLACK, Map(P44 -> BS), State.EMPTY_HANDS).getPromotionFlag(P44, P33) must be(Some(CanPromote))
    State(BLACK, Map(P77 -> BS), State.EMPTY_HANDS).getPromotionFlag(P77, P66) must be(Some(CannotPromote))
    State(WHITE, Map(P77 -> WS), State.EMPTY_HANDS).getPromotionFlag(P77, P66) must be(Some(CanPromote))
    State(WHITE, Map(P66 -> WS), State.EMPTY_HANDS).getPromotionFlag(P66, P77) must be(Some(CanPromote))

    // from hand
    State(BLACK, Map.empty, State.EMPTY_HANDS.updated(BP, 1)).getPromotionFlag(HAND, P55) must be(Some(CannotPromote))
    State(WHITE, Map.empty, State.EMPTY_HANDS.updated(WP, 1)).getPromotionFlag(HAND, P55) must be(Some(CannotPromote))
  }
  it must "return None when from is invalid" in {
    State.HIRATE.getPromotionFlag(P55, P54) must be(None)
    State.HIRATE.getPromotionFlag(P33, P34) must be(None)
  }

}