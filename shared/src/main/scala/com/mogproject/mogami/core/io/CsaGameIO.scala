package com.mogproject.mogami.core.io

import com.mogproject.mogami.core.move._
import com.mogproject.mogami.core.{Game, GameInfo, State}

import scala.annotation.tailrec

/**
  */
trait CsaGameIO {

}

/**
  * Writes Csa-formatted game
  */
trait CsaGameWriter extends CsaGameIO with CsaLike {
  def initialState: State

  def descriptiveMoves: Vector[Move]

  def gameInfo: GameInfo

  def finalAction: Option[SpecialMove]

  override def toCsaString: String =
    (gameInfo :: initialState :: (descriptiveMoves ++ finalAction).toList) map (_.toCsaString) filter (!_.isEmpty) mkString "\n"

}

/**
  * Reads Csa-formatted game
  */
trait CsaGameReader extends CsaGameIO with CsaFactory[Game] {

  private[this] def isStateText(t: String): Boolean = t.startsWith("P") || t == "+" || t == "-"

  private[this] def sectionSplitter(nel: NonEmptyLines): (Lines, NonEmptyLines, Lines, Option[Line]) = {
    val (a, ss) = nel.lines.span(ln => !isStateText(ln._1))
    val (b, c) = ss.span(ln => isStateText(ln._1))

    if (b.isEmpty) throw new RecordFormatException(a.lastOption.map(_._2).getOrElse(0), "initial state must be defined")

    (a, NonEmptyLines(b), c, None)
  }

  private[this] def parseGameInfo(lines: Lines): GameInfo = {
    @tailrec
    def f(ls: List[Line], sofar: GameInfo): GameInfo = ls match {
      case (s, n) :: xs =>
        GameInfo.keys.find { k => s.startsWith(k._2) } match {
          case Some((k, c)) => f(xs, sofar.updated(k, s.substring(c.length)))
          case None => throw new RecordFormatException(n, s"unexpected symbol: ${s}")
        }
      case _ => sofar
    }

    f(lines.toList, GameInfo())
  }

  /**
    * @param initialState initial state
    * @param lines        sequence of line expressions
    * @param footer       not used
    * @return Game instance
    */
  protected[io] def parseMoves(initialState: State, lines: Lines, footer: Option[Line]): Game = {
    @tailrec
    def f(ls: List[Line], pending: List[Line], illegal: Option[Move], sofar: Game): Game = (ls, pending, illegal) match {
      case (ln :: xs, _, _) if ln._1.startsWith("T") => f(xs, pending :+ ln, illegal, sofar)
      case (Nil, (x, n) :: _, None) if x.startsWith("%") => // evaluate special move
        val special = MoveBuilderCsa.parseTime(NonEmptyLines(pending)) match {
          case ((Resign.csaKeyword, _), tm) => Resign(tm)
          case ((TimeUp.csaKeyword, _), tm) => TimeUp(tm)
          case ((Pause.csaKeyword, _), _) => Pause
          case _ => throw new RecordFormatException(n, s"unknown special move: ${x}")
        }
        sofar.copy(finalAction = Some(special))
      case (Nil, (IllegalMove.csaKeyword, _) :: Nil, Some(mv)) => // ends with explicit illegal move
        sofar.copy(finalAction = Some(IllegalMove(mv)))
      case (Nil, Nil, Some(mv)) => // ends with implicit illegal move
        sofar.copy(finalAction = Some(IllegalMove(mv)))
      case (Nil, Nil, None) => // ends without errors
        sofar
      case (_, _, None) if pending.nonEmpty => // evaluate the pending move
        val bldr = MoveBuilderCsa.parseCsaString(NonEmptyLines(pending))
        bldr.toMove(sofar.currentState, isStrict = false) match {
          case Some(mv) => mv.verify.flatMap(sofar.makeMove) match {
            case Some(g) => f(ls, Nil, None, g) // legal move
            case None => f(ls, Nil, Some(mv), sofar) // illegal move
          }
          case None => throw new RecordFormatException(pending.head._2, s"invalid move: ${pending.head._1}")
        }
      case (ln :: xs, Nil, _) => f(xs, pending :+ ln, illegal, sofar)
      case ((x, n) :: _, _, _) => throw new RecordFormatException(n, s"unexpected move expression: ${x}")
      // $COVERAGE-OFF$
      case _ => throw new RuntimeException("never happens")
      // $COVERAGE-ON$
    }

    f(lines.toList, Nil, None, Game(initialState))
  }

  private[this] val parser = new RecordParser(sectionSplitter, parseGameInfo, State.parseCsaString, parseMoves)

  override def parseCsaString(nel: NonEmptyLines): Game = parser.parse(nel)

}