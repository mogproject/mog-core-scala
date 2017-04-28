package com.mogproject.mogami.core.io.kif

import com.mogproject.mogami.core._
import com.mogproject.mogami.core.game.{Branch, Game, GameInfo}
import com.mogproject.mogami.core.io._
import com.mogproject.mogami.core.move._
import com.mogproject.mogami.core.state.StateCache.Implicits._
import com.mogproject.mogami.core.state.{State, StateCache, StateConstant}
import com.mogproject.mogami.util.Implicits._

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

  protected def isNormalMoveKif(s: String): Boolean = s.headOption.exists(c => c == '同' || '１' <= c && c <= '９')

}

/**
  * Writes Kif-formatted game
  */
trait KifBranchWriter extends KifLike {
  def initialState: State

  def offset: Int

  def moves: Vector[Move]

  def finalAction: Option[SpecialMove]

  def comments: Map[Int, String]

  def descriptiveMoves: Vector[Move]

  override def toKifString: String = {
    def commentToSeq(index: Int): Seq[String] = for {
      text <- comments.get(index).toSeq
      line <- text.split("\n").toSeq
    } yield "*" + line

    val lines: Seq[String] = for {
      (m, i) <- ((descriptiveMoves ++ finalAction).map(_.toKifString) :+ "").zipWithIndex
      index = offset + i
      ln <- commentToSeq(index) :+ f"${index + 1}%4d ${m}"
    } yield ln

    // remove the last element (dummy)
    lines.init.mkString("\n")
  }
}

trait KifGameWriter extends KifGameIO with KifLike with Ki2Like {
  def trunk: Branch

  def branches: Vector[Branch]

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
    val t = Seq(getHeader + "\n", "手数----指手----消費時間--", trunk.toKifString)
    /** @note to keep the compatibility with KIF Format, sort the branches by offset from the largest to the smallest */
    val b = branches.sortBy(-_.offset).map(br => s"\n\n変化：${br.offset + 1}手\n" + br.toKifString)
    (t ++ b).filter(_.nonEmpty).mkString("", "\n", "\n")
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
trait KifBranchReader extends KifGameIO {

  /** (line number, (move number, move description), comment) */
  type LineInput = (Int, Option[(Int, String)], Option[String])

  /**
    * Parse Kif string as a trunk
    */
  def parseKifString(lines: Lines, initialState: State)(implicit stateCache: StateCache): Branch = {
    parseKifString(lines, (offset, _) => (Branch(stateCache.set(initialState), offset), None))
  }

  /**
    * Parse Kif string as a branch
    *
    * @note When parsing branches in KIF Format, the reader must traverse previous branches and the trunk inversely
    */
  def parseKifString(lines: Lines, trunk: Branch, branchesSofar: List[Branch])(implicit stateCache: StateCache): Branch = {
    def f(offset: Int, offsetLineNo: Int): (Branch, Option[Square]) = {
      branchesSofar.find(br => br.hasHistoryAt(offset) && br.offset != offset) match {
        case None =>
          // parent is trunk
          if (trunk.hasHistoryAt(offset)) {
            val targetMove = offset - trunk.offset - 1
            val lastMoveTo = trunk.moves.isDefinedAt(targetMove).option(trunk.moves(targetMove).to)
            (trunk.deriveNewBranch(offset).get, lastMoveTo)
          } else {
            throw new RecordFormatException(offsetLineNo, s"invalid offset for a branch: ${offset}")
          }
        case Some(parent) =>
          // parent is branch
          val targetMove = offset - parent.offset - 1
          val lastMoveTo = parent.moves.isDefinedAt(targetMove).option(parent.moves(targetMove).to)
          (parent.getSubBranch(offset).get, lastMoveTo)
      }
    }

    parseKifString(lines, f _)
  }

  /**
    * Parse Kif string
    *
    * @param createBranch a function that create a Branch instance and 'lastMoveTo' from an offset number
    */
  def parseKifString(lines: Lines, createBranch: (Int, LineNo) => (Branch, Option[Square])): Branch = {
    // convert lines
    val converted = convertLines(lines)

    val (offsetLineNo, offset) = converted.find(_._2.isDefined).map {
      case (n, Some((x, _)), _) => n -> (x - 1)
      case _ => 0 -> 0
    }.getOrElse(0 -> 0)

    val comments = parseComments(converted)

    val (br, lastMoveTo) = createBranch(offset, offsetLineNo)

    // parse moves
    parseMoves(br.updateComments(comments), lastMoveTo, converted)
  }

  protected[kif] def convertLines(lines: Lines): List[LineInput] = lines.flatMap {
    case (x, n) if x.startsWith("*") || x.startsWith("#") => List((n, None, Some(x.tail))) // comment lines
    case (x, n) =>
      x.trim.split("[ ]+", 2).toList match {
        case s :: t :: Nil => Try(s.toInt).toOption.map(i => (n, Some((i, t)), None))
        case _ => Nil
      }
    case _ => Nil
  }.toList

  protected[kif] def parseComments(input: List[LineInput]): Map[Int, String] = {
    @tailrec
    def f(ls: List[LineInput], sofar: Map[Int, String], pos: Int, remainder: List[String]): Map[Int, String] = ls match {
      case Nil => g(sofar, pos, remainder) //finish
      case (_, _, Some(s)) :: xs => f(xs, sofar, pos, s :: remainder) // comment line
      case (_, Some((p, _)), _) :: xs => f(xs, g(sofar, p - 1, remainder), p, Nil)
      case _ :: xs => f(xs, sofar, pos, remainder)
    }

    def g(sofar: Map[Int, String], pos: Int, remainder: List[String]) =
      if (remainder.nonEmpty) sofar.updated(pos, remainder.reverse.mkString("\n")) else sofar

    f(input, Map.empty, 0, Nil)
  }

  protected[kif] def parseMoves(initialBranch: Branch, initialLastMoveTo: Option[Square], input: List[LineInput]): Branch = {
    @tailrec
    def f(ls: List[LineInput], illegal: Option[(Line, Move)], sofar: Branch): Branch = (ls, illegal) match {
      case (Nil, Some((_, mv))) => // ends with implicit illegal move
        sofar.copy(finalAction = Some(IllegalMove(mv)))
      case (Nil, None) => // ends without errors
        sofar
      case ((n, Some((_, x)), _) :: Nil, None) if !isNormalMoveKif(x) => // ends with a special move
        val special = MoveBuilderKif.parseTime((x, n)) match {
          case ((Resign.kifKeyword, _), tm) => Resign(tm)
          case ((TimeUp.kifKeyword, _), tm) => TimeUp(tm)
          case ((TimeUp.kifKeyword2, _), tm) => TimeUp(tm)
          case ((Pause.kifKeyword, _), _) => Pause
          case _ => throw new RecordFormatException(n, s"unknown special move: ${x}")
        }
        sofar.copy(finalAction = Some(special))
      case ((_, Some((_, x)), _) :: Nil, Some((_, mv))) if x.startsWith(IllegalMove.kifKeyword) => // ends with explicit illegal move
        sofar.copy(finalAction = Some(IllegalMove(mv)))
      case ((n, Some((_, x)), _) :: xs, None) =>
        val bldr = MoveBuilderKif.parseKifString(NonEmptyLines(n, x))
        bldr.toMove(sofar.lastState, (sofar.lastMoveTo ++ initialLastMoveTo).headOption, isStrict = false) match {
          case Some(mv) => mv.verify.flatMap(sofar.makeMove) match {
            case Some(g) => f(xs, None, g) // legal move
            case None => f(xs, Some((x, n), mv), sofar) // illegal move
          }
          case None => throw new RecordFormatException(n, s"invalid move: ${x}")
        }
      case (_, Some(((x, n), _))) =>
        throw new RecordFormatException(n, s"invalid move expression: ${x}")
      case ((_ :: xs), _) => // ignore other lines
        f(xs, illegal, sofar)
    }

    f(input, None, initialBranch)
  }

}

trait KifGameReader extends KifGameIO with KifFactory[Game] with Ki2Factory[Game] {

  private[this] def isNormalMoveKi2(s: String): Boolean = s.headOption.exists(c => Player.symbolTable.mkString.contains(c))

  private[this] def isInitialState(s: String): Boolean = s match {
    case "  ９ ８ ７ ６ ５ ４ ３ ２ １" => true
    case _ if s.headOption.exists(List('+', '|').contains) => true
    case _ if s.slice(1, 5) == "手の持駒" => true
    case _ if s.slice(1, 3) == "手番" => true
    case _ if s.startsWith("手合割：") => true
    case _ => false
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
    (gi, st, body.drop(1), None)
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
  protected[kif] def parseMovesKif(initialState: State, lines: Lines, footer: Option[Line]): Game = {
    val ls = splitBranchesKif(lines.toList, Nil, Nil)
    val trunk = Branch.parseKifString(ls.headOption.getOrElse(Nil), initialState)
    val branches = ls.drop(1).foldLeft(List.empty[Branch]) { case (xs, ln) => Branch.parseKifString(ln, trunk, xs) :: xs }
    Game(trunk, branches.reverse.toVector)
  }

  @tailrec
  final protected[kif] def splitBranchesKif(ls: Lines, sofar: List[Lines], remainder: List[Line]): List[Lines] = (ls, remainder.nonEmpty) match {
    case (Nil, true) => splitBranchesKif(Nil, remainder.reverse :: sofar, Nil)
    case (Nil, false) => sofar.reverse
    case ((x, _) :: xs, _) if x.startsWith("変化：") => splitBranchesKif(xs, remainder.reverse :: sofar, Nil)
    case (ln :: xs, _) => splitBranchesKif(xs, sofar, ln :: remainder)
  }

  protected[kif] def parseMovesKi2(initialState: State, lines: Lines, footer: Option[Line]): Game = {
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