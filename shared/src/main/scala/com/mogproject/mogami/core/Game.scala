package com.mogproject.mogami.core

import scala.annotation.tailrec
import com.mogproject.mogami._
import com.mogproject.mogami.core.io._
import com.mogproject.mogami.core.move.MoveBuilder
import com.mogproject.mogami.util.Implicits._

import scala.util.Try

/**
  * Game
  */
case class Game(initialState: State = State.HIRATE,
                moves: Vector[move.Move] = Vector.empty,
                gameInfo: GameInfo = GameInfo(),
                movesOffset: Int = 0,
                givenHistory: Option[Vector[State]] = None
               ) extends CsaLike with SfenLike {

  require(history.length == moves.length + 1, "all moves must be valid")

  import com.mogproject.mogami.core.Game.GameStatus._

  override def equals(obj: scala.Any): Boolean = obj match {
    case that: Game =>
      // ignore givenHistory
      initialState == that.initialState && moves == that.moves && gameInfo == that.gameInfo && movesOffset == that.movesOffset
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
      Playing
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

  override def toCsaString: String =
    (gameInfo :: initialState :: moves.toList) map (_.toCsaString) filter (!_.isEmpty) mkString "\n"

  override def toSfenString: String = (initialState.toSfenString :: movesOffset.toString :: moves.map(_.toSfenString).toList).mkString(" ")

  /**
    * Check if the latest move is the repetition.
    *
    * @return true if the latest move is the repetition
    */
  def isRepetition: Boolean = hashCodes.drop(1).count(_ == currentState.hashCode()) >= 4

  def isPerpetualCheck: Boolean = currentState.isChecked &&
    (history.drop(1).reverse.takeWhile(s => s.turn == !turn || s.isChecked).count(_.hashCode() == currentState.hashCode()) >= 4)
}

object Game extends CsaFactory[Game] with SfenFactory[Game] {
  override def parseCsaString(s: String): Option[Game] = {
    def isStateText(t: String) = t.startsWith("P") || t == "+" || t == "-"

    for {
      xs <- Some(s.split('\n').toList)
      (a, ys) = xs.span(!isStateText(_))
      (b, c) = ys.span(isStateText)
      gi <- GameInfo.parseCsaString(a)
      st <- State.parseCsaString(b)
      moves = c.flatMap(s => move.MoveBuilderCsa.parseCsaString(s)) if moves.length == c.length
      game <- moves.foldLeft(Some(Game(st, Vector.empty, gi)): Option[Game])((g, m) => g.flatMap(_.makeMove(m)))
    } yield game
  }

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
}

object GameInfo extends CsaFactory[GameInfo] {
  def parseCsaString(s: String): Option[GameInfo] = {
    @tailrec
    def f(ss: List[String], sofar: Option[GameInfo]): Option[GameInfo] = (ss, sofar) match {
      case (x :: xs, Some(gt)) =>
        keys.filter { k => x.startsWith(k._2) } match {
          case (k, c) :: _ => f(xs, Some(gt.updated(k, x.substring(c.length))))
          case _ => None
        }
      case _ => sofar // (_, None) => None; (Nil, _) => sofar
    }

    f(s.isEmpty.fold(List(), s.split('\n').toList), Some(GameInfo()))
  }

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