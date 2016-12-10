package com.mogproject.mogami.core

import scala.annotation.tailrec
import com.mogproject.mogami.core.io.{CsaFactory, CsaLike, SfenFactory, SfenLike}

/**
  * Game
  */
case class Game(initialState: State = State.HIRATE, moves: Seq[Move], gameInfo: GameInfo, movesOffset: Int = 0) extends CsaLike with SfenLike {

  import com.mogproject.mogami.core.Game.GameStatus._

  /** history of states */
  lazy val history: Seq[Option[State]] = moves.scanLeft(Some(initialState): Option[State])((s, m) => s.flatMap(_.makeMove(m)))

  lazy val hashCodes: Seq[Int] = for (s <- history) yield s.map(_.hashCode()).getOrElse(0)

  lazy val status: GameStatus = if (isValid) if (isMated) Mated else Playing else InvalidState

  /**
    * Get the latest state.
    */
  def currentState: Option[State] = history.last

  def makeMove(m: Move): Game = this.copy(moves = moves :+ m)

  override def toCsaString: String =
    (gameInfo :: initialState :: moves.toList) map (_.toCsaString) filter (!_.isEmpty) mkString "\n"

  override def toSfenString: String = ???

  def isValid: Boolean = currentState.isDefined

  def isMated: Boolean = ???

  /**
    * Check if the latest move is the repetition.
    *
    * @return true if the latest move is the repetition
    */
  def isRepetition: Boolean = currentState.exists(s => hashCodes.count(_ == s.hashCode()) >= 4)
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
      // todo: parse time format (e.g. +7776FU,T12)
      moves = c.map(s => Move.parseCsaString(s.take(7))) if moves.forall(_.isDefined)
      g = Game(st, moves.flatten, gi)
      if g.isValid // check if all the moves are legal
    } yield {
      g
    }
  }

  override def parseSfenString(s: String): Option[Game] = ???

  object GameStatus extends Enumeration {
    type GameStatus = Value
    val Playing, Mated, InvalidState = Value
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

    f(if (s.isEmpty) List() else s.split('\n').toList, Some(GameInfo()))
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