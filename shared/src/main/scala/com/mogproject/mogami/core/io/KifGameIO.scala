package com.mogproject.mogami.core.io

import com.mogproject.mogami.core.move._
import com.mogproject.mogami.core.{Game, GameInfo, State, StateConstant}

import scala.annotation.tailrec
import scala.util.Try

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
trait KifGameWriter extends KifGameIO with KifLike with Ki2Like {
  def initialState: State

  def moves: Vector[Move]

  def gameInfo: GameInfo

  def movesOffset: Int

  def finalAction: Option[SpecialMove]

  def toHeaderString: String = {
    // todo: add 上手/下手
    val blackName = s"先手：${gameInfo.tags.getOrElse('blackName, "")}"
    val whiteName = s"後手：${gameInfo.tags.getOrElse('whiteName, "")}"

    getPresetLabel(initialState).map { label =>
      Seq(s"手合割：${label}", blackName, whiteName)
    }.getOrElse {
      val ss = initialState.toKifString.split('\n').toSeq
      (whiteName +: ss.take(13)) ++ (blackName +: ss.drop(13))
    }.mkString("\n")
  }

  override def toKifString: String = {
    val ms = moves.map(_.toKifString) ++ finalAction.toList.flatMap(_.toKifString.split('\n'))
    val body = ("手数----指手----消費時間--" +: ms.zipWithIndex.map { case (m, n) =>
      f"${n + movesOffset + 1}%4d ${m}"
    }).mkString("\n")
    toHeaderString + "\n\n" + body
  }

  override def toKi2String: String = {
    val ms = moves.map(m => m.player.toSymbolString(false) + m.toJapaneseNotationString)
    val body = ms.grouped(6).map(_.mkString(" ")).mkString("\n")
    toHeaderString + "\n\n" + body
  }
}

/**
  * Reads Kif-formatted game
  */
trait KifGameReader extends KifGameIO with KifFactory[Game] {

  private def isNormalMove(s: String): Boolean = s.headOption.exists(c => c == '同' || '１' <= c && c <= '９')

  private def isValidLine(s: String): Boolean = s.nonEmpty && !s.startsWith("*") && !s.startsWith("#")

  private def createChunks(xs: Seq[String]): Seq[String] = xs.flatMap { s => {
    val chunks = s.trim.split(" ", 2)
    if (chunks.length < 2 || Try(chunks(0).toInt).isFailure)
      Seq.empty
    else
      Seq(chunks(1))
  }}

  @tailrec
  final protected[io] def parseMovesKif(chunks: List[String], pending: Option[Move], sofar: Option[Game]): Option[Game] = (chunks, pending, sofar) match {
    case (x :: Nil, None, Some(g)) if !isNormalMove(x) => // ends with a special move
      MoveBuilderKif.parseTime(x) match {
        case Some((ss, tm)) =>
          (ss match {
            case Resign.kifKeyword => Some(Resign(tm))
            case TimeUp.kifKeyword => Some(TimeUp(tm))
            case _ => None // unknown command
          }).map(sm => g.copy(finalAction = Some(sm)))
        case None => None // format error
      }
    case (x :: Nil, Some(mv), Some(g)) if x.startsWith(IllegalMove.kifKeyword) => // ends with an explicit illegal move
      Some(g.copy(finalAction = Some(IllegalMove(mv))))
    case (Nil, Some(mv), Some(g)) => // ends with implicit illegal move
      Some(g.copy(finalAction = Some(IllegalMove(mv))))
    case (Nil, None, Some(g)) => sofar // ends without errors
    case (x :: xs, None, Some(g)) => MoveBuilderKif.parseKifString(x) match {
      case Some(bldr) => bldr.toMove(g.currentState, isStrict = false) match {
        case Some(mv) => mv.verify.flatMap(g.makeMove) match {
          case Some(gg) => parseMovesKif(xs, None, Some(gg)) // read the next line
          case None => parseMovesKif(xs, Some(mv), sofar)
        }
        case None =>
          None // failed to create Move
      }
      case None =>
        None // failed to parse Move string
    }
    case _ =>
      None
  }


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
      xs <- Some(s.split('\n').filter(isValidLine))
      (header, body) = xs.span(!_.startsWith("手数"))
      gi <- GameInfo.parseKifString(header.mkString("\n"))
      st <- (getPresetState(header) #:: getDefinedState(header) #:: Stream.empty).flatten.headOption
      chunks = createChunks(body.drop(1))
      game <- parseMovesKif(chunks.toList, None, Some(Game(st, Vector.empty, gi)))
    } yield game
  }
}