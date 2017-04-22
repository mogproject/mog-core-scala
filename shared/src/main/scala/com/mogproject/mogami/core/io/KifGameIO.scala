package com.mogproject.mogami.core.io

import com.mogproject.mogami.core._
import com.mogproject.mogami.core.move._
import com.mogproject.mogami.core.game.GameStatus.GameStatus
import com.mogproject.mogami.core.game.{Branch, Game, GameInfo}
import com.mogproject.mogami.core.state.{State, StateConstant}
import com.mogproject.mogami.core.state.StateCache.Implicits._

import scala.annotation.tailrec
import scala.util.Try

/**
  * Common interface for Kif and KI2 format
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
  def trunk: Branch

  def gameInfo: GameInfo

  private[this] def getHeader: String = {
    // todo: add 上手/下手
    val blackName = s"先手：${gameInfo.tags.getOrElse('blackName, "")}"
    val whiteName = s"後手：${gameInfo.tags.getOrElse('whiteName, "")}"

    getPresetLabel(trunk.initialState).map { label =>
      Seq(s"手合割：${label}", blackName, whiteName)
    }.getOrElse {
      val ss = trunk.initialState.toKifString.split('\n').toSeq
      (whiteName +: ss.take(13)) ++ (blackName +: ss.drop(13))
    }.mkString("\n")
  }

  override def toKifString: String = {
    val ms = (trunk.descriptiveMoves ++ trunk.finalAction).map(_.toKifString)
    val body = ("手数----指手----消費時間--" +: ms.zipWithIndex.map { case (m, n) =>
      f"${n + trunk.offset + 1}%4d ${m}"
    }).mkString("\n")
    getHeader + "\n\n" + body + "\n"
  }

  override def toKi2String: String = {
    val movesPerLine: Int = 6
    val ms = trunk.descriptiveMoves.map(_.toKi2String)
    val lst = trunk.finalAction.map(_.toKi2String(trunk.lastState.turn, trunk.descriptiveMoves.length))
    val body = (ms.grouped(movesPerLine).map(_.mkString(" ")) ++ lst).mkString("\n")
    getHeader + "\n\n" + body + "\n"
  }
}

/**
  * Reads Kif-formatted game
  */
trait KifGameReader extends KifGameIO with KifFactory[Game] with Ki2Factory[Game] {

  private[this] def isNormalMoveKif(s: String): Boolean = s.headOption.exists(c => c == '同' || '１' <= c && c <= '９')

  private[this] def isNormalMoveKi2(s: String): Boolean = s.headOption.exists(c => Player.symbolTable.mkString.contains(c))

  private[this] def isInitialState(s: String): Boolean = s match {
    case "  ９ ８ ７ ６ ５ ４ ３ ２ １" => true
    case _ if s.headOption.exists(List('+', '|').contains) => true
    case _ if s.slice(1, 5) == "手の持駒" => true
    case _ if s.slice(1, 3) == "手番" => true
    case _ if s.startsWith("手合割：") => true
    case _ => false
  }

  private[this] def splitMovesKif(lines: Lines): Lines = lines.flatMap { case (x, n) => {
    val chunks = x.trim.split("[ ]+", 2)
    if (chunks.length < 2 || Try(chunks(0).toInt).isFailure) Seq.empty else Seq((chunks(1), n))
  }
  }

  protected[io] def splitMovesKi2(lines: Lines): Lines = lines.flatMap {
    case (x, n) if isNormalMoveKi2(x) => x.split(" ").filter(_.nonEmpty).map((_, n))
    case _ => Seq.empty
  }

  private[this] def sectionSplitterCommon(nel: NonEmptyLines, isHeader: String => Boolean): (Lines, NonEmptyLines, Lines) = {
    val (header, body) = nel.lines.span { case (x, _) => isHeader(x) }
    val (st, gi) = header.partition(ln => isInitialState(ln._1))

    if (st.isEmpty) throw new RecordFormatException(header.lastOption.map(_._2).getOrElse(0), "initial state must be defined")
    (gi, NonEmptyLines(st), body)
  }

  private[this] def sectionSplitterKif(nel: NonEmptyLines): (Lines, NonEmptyLines, Lines, Option[Line]) = {
    val (gi, st, body) = sectionSplitterCommon(nel, { s => !s.startsWith("手数") })
    val b = body.drop(1).span { case (x, _) => !x.startsWith("変化：")}._1 // ignore branches (todo)
    (gi, st, splitMovesKif(b), None)
  }

  private[this] def sectionSplitterKi2(nel: NonEmptyLines): (Lines, NonEmptyLines, Lines, Option[Line]) = {
    val (gi, st, body) = sectionSplitterCommon(nel, { s => !isNormalMoveKi2(s) })
    (gi, st, splitMovesKi2(body), body.lastOption.find(_._1.startsWith("まで")))
  }

  // todo: implement more traits
  private[this] def parseGameInfo(lines: Lines): GameInfo = {
    @tailrec
    def f(ls: List[Line], sofar: GameInfo): GameInfo = ls match {
      case (x, n) :: xs =>
        if (x.startsWith("先手：") || x.startsWith("下手："))
          f(xs, sofar.updated('blackName, x.drop(3)))
        else if (x.startsWith("後手：") || x.startsWith("上手："))
          f(xs, sofar.updated('whiteName, x.drop(3)))
        else // ignore other flags
          f(xs, sofar)
      case _ => sofar
    }

    f(lines.toList, GameInfo())
  }

  private[this] def parseInitialState(nel: NonEmptyLines): State = {
    if (nel.lines.last._1.startsWith("手合割：")) {
      // preset state
      val (x, n) = nel.lines.last
      presetStates.getOrElse(x.drop(4), throw new RecordFormatException(n, s"unknown preset state: ${x.drop(4)}"))
    } else {
      State.parseKifString(nel)
    }
  }

  /**
    * @param initialState initial state
    * @param lines        sequence of line expressions
    * @param footer       not used
    * @return Game instance
    */
  // todo: refactor these functions
  protected[io] def parseMovesKif(initialState: State, lines: Lines, footer: Option[Line]): Game = {
    @tailrec
    def f(ls: List[Line], illegal: Option[(Line, Move)], sofar: Branch): Branch = (ls, illegal) match {
      case ((x, n) :: Nil, None) if !isNormalMoveKif(x) => // ends with a special move
        val special = MoveBuilderKif.parseTime((x, n)) match {
          case ((Resign.kifKeyword, _), tm) => Resign(tm)
          case ((TimeUp.kifKeyword, _), tm) => TimeUp(tm)
          case ((TimeUp.kifKeyword2, _), tm) => TimeUp(tm)
          case ((Pause.kifKeyword, _), _) => Pause
          case _ => throw new RecordFormatException(n, s"unknown special move: ${x}")
        }
        sofar.copy(finalAction = Some(special))
      case ((x, _) :: Nil, Some((_, mv))) if x.startsWith(IllegalMove.kifKeyword) => // ends with explicit illegal move
        sofar.copy(finalAction = Some(IllegalMove(mv)))
      case (Nil, Some((_, mv))) => // ends with implicit illegal move
        sofar.copy(finalAction = Some(IllegalMove(mv)))
      case (Nil, None) => sofar // ends without errors
      case ((x, n) :: xs, None) =>
        val bldr = MoveBuilderKif.parseKifString(NonEmptyLines(n, x))
        bldr.toMove(sofar.lastState, sofar.lastMoveTo, isStrict = false) match {
          case Some(mv) => mv.verify.flatMap(sofar.makeMove) match {
            case Some(g) => f(xs, None, g) // legal move
            case None => f(xs, Some((x, n), mv), sofar) // illegal move
          }
          case None => throw new RecordFormatException(n, s"invalid move: ${x}")
        }
      case (_ :: _, Some(((x, n), _))) => throw new RecordFormatException(n, s"invalid move expression: ${x}")
    }

    val trunk = f(lines.toList, None, Branch(initialState))
    Game(trunk)
  }

  protected[io] def parseMovesKi2(initialState: State, lines: Lines, footer: Option[Line]): Game = {
    @tailrec
    def f(ls: List[Line], illegal: Option[(Line, Move)], sofar: Branch): Branch = (ls, illegal, footer) match {
      case (Nil, Some((_, mv)), Some((x, n))) if x.contains(IllegalMove.ki2Keyword) => // ends with explicit illegal move
        sofar.copy(finalAction = Some(IllegalMove(mv)))
      case (Nil, Some((_, mv)), None) => // ends with implicit illegal move
        sofar.copy(finalAction = Some(IllegalMove(mv)))
      case (Nil, None, Some((x, n))) => // ends with a special move except illegal move
        val special = if (x.contains(TimeUp.ki2Keyword)) {
          TimeUp()
        } else if (x.contains(IllegalMove.ki2Keyword)) {
          throw new RecordFormatException(n, s"unexpected illegal move: ${x}")
        } else if (x.contains("勝ち")) {
          Resign()
        } else if (x.contains(Pause.ki2Keyword)) {
          Pause
        } else {
          throw new RecordFormatException(n, s"unknown special move: ${x}")
        }
        sofar.copy(finalAction = Some(special))
      case (Nil, None, None) => sofar // ends without errors
      case ((x, n) :: xs, None, _) =>
        val bldr = MoveBuilderKi2.parseKi2String(NonEmptyLines(n, x))
        bldr.toMove(sofar.lastState, sofar.lastMoveTo, isStrict = false) match {
          case Some(mv) => mv.verify.flatMap(sofar.makeMove) match {
            case Some(g) => f(xs, None, g) // legal move
            case None => f(xs, Some((x, n), mv), sofar) // illegal move
          }
          case None => throw new RecordFormatException(n, s"invalid or ambiguous move: ${x}")
        }
      case (_ :: _, Some(((x, n), _)), _) => throw new RecordFormatException(n, s"invalid move expression: ${x}")
    }

    val trunk = f(lines.toList, None, Branch(initialState))
    Game(trunk)
  }

  private[this] val parserKif = new RecordParser(sectionSplitterKif, parseGameInfo, parseInitialState, parseMovesKif)

  private[this] val parserKi2 = new RecordParser(sectionSplitterKi2, parseGameInfo, parseInitialState, parseMovesKi2)

  override def parseKifString(nel: NonEmptyLines): Game = parserKif.parse(nel)

  override def parseKi2String(nel: NonEmptyLines): Game = parserKi2.parse(nel)

}