package com.mogproject.mogami.core.game

import com.mogproject.mogami.core.Ptype.PAWN
import com.mogproject.mogami.core.Square
import com.mogproject.mogami.core.game.GameStatus.GameStatus
import com.mogproject.mogami.core.game.GameStatus._
import com.mogproject.mogami.core.io.sfen.{SfenBranchReader, SfenBranchWriter}
import com.mogproject.mogami.core.move._
import com.mogproject.mogami.core.state.{State, StateCache, StateHash}
import com.mogproject.mogami.core.state.StateHash.StateHash
import com.mogproject.mogami.util.Implicits._


/**
  * Manages one branch
  */
case class Branch(initialHash: StateHash,
                  offset: Int = 0,
                  moves: Vector[Move] = Vector.empty,
                  finalAction: Option[SpecialMove] = None,
                  comments: Map[Int, String] = Map.empty,
                  hint: Option[BranchHint] = None)
                 (implicit stateCache: StateCache) extends SfenBranchWriter {

  require(history.length == moves.length + 1, "all moves must be valid")


  /**
    * Overrides '==' method
    *
    * @note ignores hint parameter
    */
  override def equals(obj: scala.Any): Boolean = obj match {
    case that: Branch => initialHash == that.initialHash && moves == that.moves && finalAction == that.finalAction && comments == that.comments
    case _ => false
  }

  def updateFinalAction(fa: Option[SpecialMove]): Branch = Branch(initialHash, offset, moves, fa, comments, Some(BranchHint(history)))

  def updateComments(cmt: Map[Int, String]): Branch = Branch(initialHash, offset, moves, finalAction, cmt, Some(BranchHint(history)))

  def updateComment(pos: Int, text: String): Branch = if (text.isEmpty) clearComment(pos) else updateComments(comments.updated(pos, text))

  def clearComment(pos: Int): Branch = updateComments(comments - pos)

  /**
    * history of state hashes
    */
  lazy val history: Vector[StateHash] = hint.map(_.history).getOrElse(createHistory())

  def initialState: State = stateCache(initialHash)

  lazy val lastState: State = stateCache(history.last)

  def lastMove: Option[Move] = moves.lastOption

  def lastMoveTo: Option[Square] = lastMove.map(_.to)

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
      case _ =>
        if (lastState.isMated) {
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
  }

  /**
    * Check if the latest move is the repetition.
    *
    * @return true if the latest move is the repetition
    */
  def isRepetition: Boolean = history.drop(1).count(_ == lastState.hash) >= 4

  def isPerpetualCheck: Boolean = lastState.isChecked &&
    (history.drop(1).reverse.takeWhile { h => !StateHash.isSamePlayer(h, lastState.hash) || stateCache(h).isChecked }.count(_ == lastState.hash) >= 4)

  /**
    * Moves for description. This includes an illegal move if it exists.
    *
    * @return vector of moves for description
    */
  def descriptiveMoves: Vector[Move] = finalAction match {
    case Some(IllegalMove(mv)) => moves :+ mv
    case _ => moves
  }

  def makeMove(move: Move): Option[Branch] = (status == Playing && lastState.isValidMove(move)).
    option(this.copy(moves = moves :+ move, hint = lastState.makeMove(move).map(s => BranchHint(history :+ stateCache.set(s)))))


  def makeMove(move: MoveBuilder): Option[Branch] = move.toMove(lastState, lastMoveTo).flatMap(makeMove)

}

object Branch extends SfenBranchReader {
  def apply()(implicit stateCache: StateCache): Branch = apply(State.HIRATE)

  def apply(state: State)(implicit stateCache: StateCache): Branch = Branch(stateCache.set(state))
}

case class BranchHint(history: Vector[StateHash])