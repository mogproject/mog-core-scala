package com.mogproject.mogami.core.io

import com.mogproject.mogami.core.move.{Move, MoveBuilderKif}
import com.mogproject.mogami.core.{Game, GameInfo, State, StateConstant}

/**
  * Common interface for Kif format
  */
trait KifGameIO {
  protected val presetStates: Map[String, State] = Map(
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

  protected def getPresetLabel(state: State): Option[String] = presetFindTable.get(state.hashCode())
}

/**
  * Writes Kif-formatted game
  */
trait KifGameWriter extends KifGameIO with KifLike {
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
      f"${n + movesOffset + 1}%4d ${m.toKifString}"
    }

    (header ++ body).mkString("\n")
  }
}

/**
  * Reads Kif-formatted game
  */
trait KifGameReader extends KifGameIO with KifFactory[Game] {
  override def parseKifString(s: String): Option[Game] = {
    def getPresetState(ls: Seq[String]): Option[State] =
      ls.withFilter(_.startsWith("手合割：")).flatMap(ss => presetStates.get(ss.drop(4))).headOption

    def getDefinedState(ls: Seq[String]): Option[State] = State.parseKifString(ls.filter {
      case "  ９ ８ ７ ６ ５ ４ ３ ２ １" => true
      case ss if ss.headOption.exists(List('+', '|').contains) => true
      case ss if ss.slice(1, 5) == "手の持駒" => true
      case ss if ss.slice(1, 3) == "手番" => true
      case _ => false
    }.mkString("\n"))

    for {
      xs <- Some(s.split('\n').filter(s => !s.startsWith("*") && !s.startsWith("#"))) // ignore comment lines
      (header, body) = xs.span(!_.startsWith("手数"))
      gi <- GameInfo.parseKifString(header.mkString("\n"))
      st <- (getPresetState(header) #:: getDefinedState(header) #:: Stream.empty).flatten.headOption
      moves = body.drop(1).flatMap(s => MoveBuilderKif.parseKifString(s.trim.split(" ", 2).drop(1).mkString))
      game <- moves.foldLeft[Option[Game]](Some(Game(st, Vector.empty, gi)))((g, m) => g.flatMap(_.makeMove(m)))
    } yield game
  }
}