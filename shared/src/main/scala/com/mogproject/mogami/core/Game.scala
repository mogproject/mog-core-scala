package com.mogproject.mogami.core

import com.mogproject.mogami._
import com.mogproject.mogami.core.io._
import com.mogproject.mogami.core.move._
import com.mogproject.mogami.util.Implicits._

import scala.annotation.tailrec
import scala.util.Try

/**
  * Game
  */
case class Game(initialState: State = State.HIRATE,
                moves: Vector[Move] = Vector.empty,
                gameInfo: GameInfo = GameInfo(),
                movesOffset: Int = 0,
                finalAction: Option[SpecialMove] = None,
                givenHistory: Option[Vector[State]] = None
               ) extends CsaGameWriter with SfenLike with KifGameWriter {

  require(history.length == moves.length + 1, "all moves must be valid")

  import com.mogproject.mogami.core.Game.GameStatus._

  override def equals(obj: scala.Any): Boolean = obj match {
    case that: Game =>
      // ignore givenHistory
      initialState == that.initialState &&
        moves == that.moves &&
        gameInfo == that.gameInfo &&
        movesOffset == that.movesOffset &&
        finalAction == that.finalAction
    case _ => false
  }

  /** history of states */
  lazy val history: Vector[State] = {
    givenHistory.getOrElse(moves.scanLeft(Some(initialState): Option[State])((s, m) => s.flatMap(_.makeMove(m))).flatten)
  }

  lazy val hashCodes: Vector[Int] = history.map(_.hashCode())

  lazy val status: GameStatus = {
    if (currentState.isMated) {
      if (lastMove.exists(m => m.isCheck && m.isDrop && m.oldPtype == PAWN))
        Uchifuzume
      else
        Mated
    } else if (isPerpetualCheck) {
      PerpetualCheck
    } else if (isRepetition) {
      Drawn // Sennichite
    } else {
      finalAction match {
        case Some(IllegalMove(_)) => IllegallyMoved
        case Some(Resign(_)) => Resigned
        case Some(TimeUp(_)) => TimedUp
        case _ => Playing
      }
    }
  }

  /**
    * Get the latest state.
    */
  def currentState: State = history.last

  def lastMove: Option[Move] = moves.lastOption

  def turn: Player = currentState.turn

  def makeMove(move: Move): Option[Game] = {
    (status == Playing && currentState.isValidMove(move)).option(this.copy(moves = moves :+ move, givenHistory = currentState.makeMove(move).map(history :+ _)))
  }

  def makeMove(move: MoveBuilder): Option[Game] = move.toMove(currentState).flatMap(makeMove)

  override def toSfenString: String = (initialState.toSfenString :: movesOffset.toString :: moves.map(_.toSfenString).toList).mkString(" ")

  /**
    * Check if the latest move is the repetition.
    *
    * @return true if the latest move is the repetition
    */
  def isRepetition: Boolean = hashCodes.drop(1).count(_ == currentState.hashCode()) >= 4

  def isPerpetualCheck: Boolean = currentState.isChecked &&
    (history.drop(1).reverse.takeWhile(s => s.turn == !turn || s.isChecked).count(_.hashCode() == currentState.hashCode()) >= 4)

  /**
    * Moves for description. This includes an illegal move if it exists.
    *
    * @return vector of moves for description
    */
  def descriptiveMoves: Vector[Move] = finalAction match {
    case Some(IllegalMove(mv)) => moves :+ mv
    case _ => moves
  }
}

object Game extends CsaGameReader with SfenFactory[Game] with KifGameReader {

  override def parseSfenString(s: String): Option[Game] = {
    val tokens = s.split(" ")

    for {
      st <- State.parseSfenString(tokens.take(3).mkString(" ")) if tokens.length >= 4
      offset <- Try(tokens(3).toInt).toOption
      gi = GameInfo() // initialize without information
      moves = tokens.drop(4).flatMap(ss => move.MoveBuilderSfen.parseSfenString(ss)) if moves.length == tokens.length - 4
      game <- moves.foldLeft(Some(Game(st, Vector.empty, gi, offset)): Option[Game])((g, m) => g.flatMap(_.makeMove(m)))
    } yield game
  }

  object GameStatus {

    sealed trait GameStatus

    case object Playing extends GameStatus

    case object Mated extends GameStatus

    case object Uchifuzume extends GameStatus

    case object PerpetualCheck extends GameStatus

    case object Drawn extends GameStatus

    case object TimedUp extends GameStatus

    case object IllegallyMoved extends GameStatus

    case object Resigned extends GameStatus

  }

}


/**
  * Game information
  */
case class GameInfo(tags: Map[Symbol, String] = Map()) extends CsaLike {

  require(validateTagKeys)

  def validateTagKeys: Boolean = tags.keys forall { k => GameInfo.keys.map(_._1).contains(k) }

  def updated(key: Symbol, value: String): GameInfo = GameInfo(tags.updated(key, value))

  def toCsaString: String = {
    GameInfo.keys.toList.flatMap {
      case (k, c) if tags.contains(k) => List(c + tags(k))
      case _ => Nil
    } mkString "\n"
  }

  // todo: impl toKifString
  /*
  example:

#KIF version=2.0 encoding=UTF-8
開始日時：2017/03/13 ??:??
終了日時：2017/03/13 ??:??
場所：81Dojo (ver.2016/03/20)
持ち時間：15分+60秒
手合割：平手
先手：black
後手：white
   */
}

object GameInfo {
  /** pairs of a symbol name and its csa-formatted string */
  val keys: Seq[(Symbol, String)] = Seq(
    ('formatVersion, "V"),
    ('blackName, "N+"),
    ('whiteName, "N-"),
    ('event, "$EVENT:"),
    ('site, "$SITE:"),
    ('startTime, "$START_TIME:"),
    ('endTime, "$END_TIME:"),
    ('timeLimit, "$TIME_LIMIT:"),
    ('opening, "$OPENING:")
  )
}