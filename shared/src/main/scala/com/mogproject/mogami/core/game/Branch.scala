package com.mogproject.mogami.core.game

import com.mogproject.mogami.core.Square
import com.mogproject.mogami.core.game.Game.{HistoryHash, Position}
import com.mogproject.mogami.core.game.GameStatus.GameStatus
import com.mogproject.mogami.core.game.GameStatus._
import com.mogproject.mogami.core.io._
import com.mogproject.mogami.core.io.html.HtmlBranchWriter
import com.mogproject.mogami.core.move._
import com.mogproject.mogami.core.state.{State, StateCache, StateHash}
import com.mogproject.mogami.core.state.StateHash.StateHash
import com.mogproject.mogami.util.BitOperation
import com.mogproject.mogami.util.Implicits._

/**
  * Manages one branch
  *
  * @param initialHash        initial status hash
  * @param offset             offset
  * @param moves              moves
  * @param finalAction        final action
  * @param initialHistoryHash not used when hint is given
  * @param hint               hint
  * @param stateCache         state cache
  */
case class Branch(initialHash: StateHash,
                  offset: Int = 0,
                  moves: Vector[Move] = Vector.empty,
                  finalAction: Option[SpecialMove] = None,
                  initialHistoryHash: Option[HistoryHash] = None,
                  hint: Option[BranchHint] = None)
                 (implicit stateCache: StateCache) extends SfenBranchWriter with HtmlBranchWriter {

  require(history.length == moves.length + 1, s"all moves must be valid: history.length=${history.length}, moves.length=${moves.length}")


  /**
    * Overrides '==' method
    *
    * @note ignores hint parameter
    */
  override def equals(obj: scala.Any): Boolean = obj match {
    case that: Branch => initialHash == that.initialHash && moves == that.moves && finalAction == that.finalAction && initialHistoryHash == that.initialHistoryHash
    case _ => false
  }

  def updateFinalAction(fa: Option[SpecialMove]): Branch = Branch(initialHash, offset, moves, fa, initialHistoryHash, Some(BranchHint(history, historyHash)))

  def getStateHash(pos: Position): Option[StateHash] = history.get(pos - offset)

  def getState(pos: Position): Option[State] = history.get(pos - offset).flatMap(stateCache.get)

  /**
    * history of state hashes
    */
  lazy val history: Vector[StateHash] = hint.map(_.history).getOrElse(createHistory())

  /**
    * hashes of history
    */
  lazy val historyHash: Vector[HistoryHash] = hint.map(_.historyHash).getOrElse(createHistoryHash())

  def initialState: State = stateCache(initialHash)

  def getMove(pos: Position): Option[Move] = moves.get(pos - offset)

  lazy val lastState: State = stateCache(history.last)

  def lastMove: Option[Move] = moves.lastOption

  def lastMoveTo: Option[Square] = lastMove.map(_.to)

  def lastLegalMoves: Vector[Move] = lastState.legalMoves(lastMoveTo)

  private[this] def createHistory(): Vector[StateHash] = {
    moves.scanLeft(Some(initialHash): Option[StateHash]) { (h, m) =>
      for {
        hash <- h
        st <- stateCache.get(hash)
        nxt <- st.makeMove(m)
      } yield {
        stateCache.set(nxt)
      }
    }.flatten
  }

  private[this] def createHistoryHash(): Vector[HistoryHash] = {
    val h0 = initialHistoryHash.getOrElse(createHistoryHash(offset, initialHash))
    history.zipWithIndex.tail.scanLeft(h0) { case (r, (h, i)) => r ^ createHistoryHash(i + offset, h) }
  }

  /**
    * Numeric hash function
    */
  private[this] def createHistoryHash(position: Position, stateHash: StateHash): HistoryHash =
    BitOperation.rotateShift(stateHash, position % 63) ^ (stateHash * (10000 + position))

  /**
    * Create SFEN string
    */
  /**
    * status
    */
  lazy val status: GameStatus = {
    finalAction match {
      case Some(IllegalMove(_)) => IllegallyMoved
      case Some(Resign(_)) => Resigned
      case Some(TimeUp(_)) => TimedUp
      case Some(DeclareWin(_)) => Jishogi
      case _ =>
        if (lastState.isMated) {
          if (lastMove.exists(m => m.isCheck && m.isPawnDrop))
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
  }

  /**
    * Check if the latest move is the repetition.
    *
    * @return true if the latest move is the repetition
    */
  def isRepetition: Boolean = history.tail.count(_ == lastState.hash) >= 4

  def isPerpetualCheck: Boolean = lastState.isChecked &&
    (history.tail.reverse.takeWhile { h => !StateHash.isSamePlayer(h, lastState.hash) || stateCache(h).isChecked }.count(_ == lastState.hash) >= 4)

  /**
    * Moves for description. This includes an illegal move if it exists.
    *
    * @return vector of moves for description
    */
  def descriptiveMoves: Vector[Move] = finalAction match {
    case Some(IllegalMove(mv)) => moves :+ mv
    case _ => moves
  }

  def makeMove(move: Move): Option[Branch] = {
    (StateHash.getNextStateHash(lastState, move) match {
      case h if stateCache.hasKey(h) => Some(h)
      case _ if status == Playing => lastState.makeMove(move).map(stateCache.set)
      case _ => None
    }).map { h =>
      this.copy(
        moves = moves :+ move,
        hint = Some(BranchHint(history :+ h, historyHash :+ (historyHash.last ^ createHistoryHash(historyHash.length + offset, h))))
      )
    }
  }

  def makeMove(move: MoveBuilder, lastMoveToValue: Option[Square] = lastMoveTo): Option[Branch] =
    move.toMove(lastState, lastMoveToValue.isDefined.fold(lastMoveToValue, lastMoveTo)).flatMap(makeMove)

  /**
    * Check if the position is valid
    *
    * @return true if valid
    */
  def hasHistoryAt(pos: Position): Boolean = history.isDefinedAt(pos - offset)

  /**
    * Create part of this branch from the initial position
    *
    * @param pos offset position
    * @return None if the position is invalid
    */
  def getSubBranch(pos: Position): Option[Branch] = {
    val relPos = pos - offset
    history.isDefinedAt(relPos).option(
      this.copy(
        moves = moves.take(relPos),
        finalAction = None,
        hint = Some(BranchHint(history.take(relPos + 1), historyHash.take(relPos + 1)))
      )
    )
  }

  def getHistoryHash(pos: Position): Option[HistoryHash] = historyHash.get(pos - offset)

  /**
    * Derive a new branch from a specific position
    *
    * @param pos offset position
    * @return None if the position is invalid
    */
  def deriveNewBranch(pos: Position): Option[Branch] = {
    val index = pos - offset
    history.get(index).map(h => Branch(h, pos, initialHistoryHash = historyHash.get(index)))
  }

  /**
    * Create a truncated branch
    *
    * Unused comments and unconnected branches are removed.
    *
    * @param pos the point to truncate
    */
  def truncated(pos: Position): Branch = {
    val relPos = math.max(0, pos - offset)
    copy(
      moves = moves.take(relPos),
      finalAction = None,
      hint = Some(BranchHint(history.take(relPos + 1), historyHash.take(relPos + 1)))
    )
  }

  /**
    * Create a map of the history hash and its next move.
    */
  def getNextMoveList: Map[HistoryHash, Move] = historyHash.zip(moves).toMap
}

object Branch extends SfenBranchReader {
  def apply()(implicit stateCache: StateCache): Branch = apply(State.HIRATE)

  def apply(state: State)(implicit stateCache: StateCache): Branch = Branch(stateCache.set(state))
}

case class BranchHint(history: Vector[StateHash], historyHash: Vector[HistoryHash])