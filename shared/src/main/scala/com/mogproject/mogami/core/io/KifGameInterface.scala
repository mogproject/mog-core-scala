package com.mogproject.mogami.core.io

import com.mogproject.mogami.core.move.Move
import com.mogproject.mogami.core.{GameInfo, State, StateConstant}

/**
  * Common interface for Kif format
  */
trait KifGameInterface {
  val presetStates: Map[String, State] = Map(
    "平手" -> StateConstant.HIRATE,
    "香落ち" -> StateConstant.HANDICAP_LANCE,
    "右香落ち" -> StateConstant.HANDICAP_LANCE_RIGHT,
    "角落ち" -> StateConstant.HANDICAP_BISHOP,
    "飛車落ち" -> StateConstant.HANDICAP_ROOK,
    "飛香落ち" -> StateConstant.HANDICAP_ROOK_LANCE,
    "二枚落ち" -> StateConstant.HANDICAP_2_PIECE,
    "三枚落ち" -> StateConstant.HANDICAP_3_PIECE,
    "四枚落ち" -> StateConstant.HANDICAP_4_PIECE,
    "五枚落ち" -> StateConstant.HANDICAP_5_PIECE,
    "左五枚落ち" -> StateConstant.HANDICAP_5_PIECE_LEFT,
    "六枚落ち" -> StateConstant.HANDICAP_6_PIECE,
    "八枚落ち" -> StateConstant.HANDICAP_8_PIECE,
    "十枚落ち" -> StateConstant.HANDICAP_10_PIECE
  )

  private[this] val presetFindTable: Map[Int, String] = presetStates.map { case (k, v) => v.hashCode() -> k }

  def getPresetLabel(state: State): Option[String] = presetFindTable.get(state.hashCode())
}

/**
  * Writes Kif-formatted game
  */
trait KifGameWriter extends KifGameInterface with KifLike {
  def initialState: State

  def moves: Vector[Move]

  def gameInfo: GameInfo

  def movesOffset: Int

  override def toKifString: String = {
    // todo: add 上手/下手
    val blackName = s"先手：${gameInfo.tags.getOrElse('blackName, "")}"
    val whiteName = s"後手：${gameInfo.tags.getOrElse('whiteName, "")}"

    val header = getPresetLabel(initialState).map { label =>
      Seq(s"手合割：${label}", blackName, whiteName)
    }.getOrElse {
      val ss = initialState.toKifString.split('\n').toSeq
      (whiteName +: ss.take(13)) ++ (blackName +: ss.drop(13))
    }

    val body = "手数----指手----消費時間--" +: moves.zipWithIndex.map { case (m, n) =>
        f"${(n + movesOffset + 1)}%4d ${m.toKifString}"
    }

    (header ++ body).mkString("\n")
  }
}
