package com.mogproject.mogami.core.io.sfen

import com.mogproject.mogami.core.game.{Branch, Game}
import com.mogproject.mogami.core.io.RecordFormatException
import com.mogproject.mogami.core.move.{Move, MoveBuilderSfen, SpecialMove}
import com.mogproject.mogami.core.state.{State, StateCache}
import com.mogproject.mogami.core.state.StateHash.StateHash

import scala.util.{Failure, Success, Try}

/**
  *
  */
case class SfenExtendedBranch(moves: String, finalAction: Option[String], comments: Map[Int, String])

case class SfenExtendedGame(trunk: SfenExtendedBranch, branches: Vector[SfenExtendedBranch])

trait SfenBranchReader {
  /**
    * Parse trunk description
    *
    * @param s "{board} {turn} {hand} {offset} [{move}...]"
    */
  def parseSfenString(s: String)(implicit stateCache: StateCache): Branch = {
    val tokens = s.split(" ")
    parseOffsetAndMoves(tokens.drop(3), Int.MaxValue, _ => Branch(State.parseSfenString(tokens.take(3).mkString(" "))).initialHash)
  }

  /**
    * Parse branch description
    *
    * @param trunk trunk
    * @param s     "{offset} [{move}...]"
    */
  def parseSfenString(trunk: Branch, s: String)(implicit stateCache: StateCache): Branch =
    parseOffsetAndMoves(tokens = s.split(" "), trunk.history.length, trunk.history.apply)

  // helper functions
  private[this] def parseOffsetAndMoves(tokens: Seq[String], maxOffsetValue: Int, initialStateFunc: Int => StateHash)
                                       (implicit stateCache: StateCache): Branch = {
    val offset = tokens.headOption.map(parseOffset(_, maxOffsetValue)).getOrElse(throw new RecordFormatException(1, s"cannot find offset"))
    val moves = tokens.drop(1).map(MoveBuilderSfen.parseSfenString)

    moves.foldLeft[Branch](Branch(initialStateFunc(offset), offset)) { (br, m) =>
      br.makeMove(m).getOrElse(throw new RecordFormatException(1, s"invalid move: ${m.toSfenString}"))
    }
  }

  private[this] def parseOffset(s: String, maxValue: Int = Int.MaxValue): Int = Try(s.toInt) match {
    case Success(n) if 0 <= n && n < maxValue => n
    case Success(_) => throw new RecordFormatException(1, s"offset must be non-negative and less than ${maxValue}: ${s}")
    case Failure(_) => throw new RecordFormatException(1, s"offset must be number: ${s}")
  }

  /**
    * Parse trunk structure
    */
  def parseSfenExtendedBranch(sfenExtendedBranch: SfenExtendedBranch)(implicit stateCache: StateCache): Branch =
    parseSfenExtendedBranchHelper(parseSfenString(sfenExtendedBranch.moves), sfenExtendedBranch)

  /**
    * Parse branch structure
    */
  def parseSfenExtendedBranch(trunk: Branch, sfenExtendedBranch: SfenExtendedBranch)(implicit stateCache: StateCache): Branch =
    parseSfenExtendedBranchHelper(parseSfenString(trunk, sfenExtendedBranch.moves), sfenExtendedBranch)

  // helper function
  private[this] def parseSfenExtendedBranchHelper(branch: Branch, sfenExtendedBranch: SfenExtendedBranch): Branch = {
    val finalAction = sfenExtendedBranch.finalAction.map(SpecialMove.parseSfenExtended(_, branch.lastState, branch.lastMoveTo))
    val comments = sfenExtendedBranch.comments
    branch.updateFinalAction(finalAction).updateComments(comments)
  }

}

trait SfenGameReader {
  def parseSfenString(s: String)(implicit stateCache: StateCache): Game = Game(Branch.parseSfenString(s))

  def parseSfenExtendedGame(sfenExtendedGame: SfenExtendedGame)(implicit stateCache: StateCache): Game = {
    val trunk = Branch.parseSfenExtendedBranch(sfenExtendedGame.trunk)
    val branches = sfenExtendedGame.branches.map(Branch.parseSfenExtendedBranch(trunk, _))
    Game(trunk, branches)
  }
}

trait SfenBranchWriter extends SfenLike {
  def initialState: State

  def offset: Int

  def moves: Vector[Move]

  def finalAction: Option[SpecialMove]

  def comments: Map[Int, String]

  /**
    * Make part of Sfen string
    *
    * @return "{offset} [{move}...]"
    */
  override def toSfenString: String = offset.toString + " " + moves.map(_.toSfenString).mkString(" ")

  def toSfenExtendedBranch: SfenExtendedBranch = SfenExtendedBranch(toSfenString, finalAction.map(_.toSfenExtendedString), comments)
}

trait SfenGameWriter extends SfenLike {
  def trunk: Branch

  def branches: Vector[Branch]

  /**
    * Make trunk Sfen string
    *
    * @return "{board} {turn} {hand} {offset} [{move}...]"
    */
  override def toSfenString: String = trunk.initialState.toSfenString + " " + trunk.toSfenString

  def toSfenExtendedGame: SfenExtendedGame = SfenExtendedGame(trunk.toSfenExtendedBranch, branches.map(_.toSfenExtendedBranch))
}