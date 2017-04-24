package com.mogproject.mogami.core.io.sfen

import com.mogproject.mogami.core.game.{Branch, Game}
import com.mogproject.mogami.core.io.RecordFormatException
import com.mogproject.mogami.core.move.{Move, MoveBuilderSfen, SpecialMove}
import com.mogproject.mogami.core.state.{State, StateCache}
import com.mogproject.mogami.core.state.StateHash.StateHash
import com.mogproject.mogami.util.Implicits._

import scala.util.{Success, Try}

/**
  *
  */
trait SfenBranchReader {
  /**
    * Parse trunk description
    *
    * @param s "{board} {turn} {hand} {offset} [{move}...]"
    */
  def parseSfenString(s: String)(implicit stateCache: StateCache): Branch = {
    val tokens = s.split(" ")
    parseSfenStringHelper(tokens.drop(3), None, _ => Branch(State.parseSfenString(tokens.take(3).mkString(" "))).initialHash)
  }

  /**
    * Parse branch description
    *
    * @param trunk trunk
    * @param s     "{offset} [{move}...]"
    */
  def parseSfenString(trunk: Branch, s: String)(implicit stateCache: StateCache): Branch =
    parseSfenStringHelper(tokens = s.split(" "), Some(Range(trunk.offset, trunk.offset + trunk.history.length)), i => trunk.history(i - trunk.offset))

  // helper functions
  private[this] def parseSfenStringHelper(tokens: Seq[String], range: Option[Range], initialStateFunc: Int => StateHash)
                                         (implicit stateCache: StateCache): Branch = {
    val offset = tokens.headOption.map(parseOffset(_, range)).getOrElse(throw new RecordFormatException(1, s"cannot find offset"))
    val moves = tokens.drop(1).map(MoveBuilderSfen.parseSfenString)

    moves.foldLeft[Branch](Branch(initialStateFunc(offset), offset)) { (br, m) =>
      br.makeMove(m).getOrElse(throw new RecordFormatException(1, s"invalid move: ${m.toSfenString}"))
    }
  }

  private[this] def parseOffset(s: String, range: Option[Range] = None): Int = (Try(s.toInt), range) match {
    case (Success(n), None) if 0 <= n => n
    case (Success(n), Some(r)) if r.contains(n) => n
    case (Success(_), _) => throw new RecordFormatException(1, s"offset is out of range: ${s}")
    case _ => throw new RecordFormatException(1, s"offset must be number: ${s}")
  }

  /**
    * Parse USEN string as a trunk
    */
  def parseUsenString(s: String, initialState: State)(implicit stateCache: StateCache): Branch =
    parseUsenStringHelper(s, _ => stateCache.set(initialState), None)

  /**
    * Parse USEN string as a branch
    */
  def parseUsenString(s: String, trunk: Branch)(implicit stateCache: StateCache): Branch =
    parseUsenStringHelper(s, i => trunk.history(i - trunk.offset), Some(Range(trunk.offset, trunk.offset + trunk.history.length)))

  // helper function
  private[this] def parseUsenStringHelper(s: String, initialStateFunc: Int => StateHash, offsetRange: Option[Range])
                                         (implicit stateCache: StateCache): Branch = {
    val tokens = s.split("[.]", 3)
    if (tokens.length != 3) throw new RecordFormatException(1, s"branch description must have three sections: ${s}")
    val Array(os, mvs, fa) = tokens

    // parse offset
    val offset = parseOffset(os, offsetRange)

    if (mvs.length % 3 != 0) throw new RecordFormatException(1, s"each move must be three characters: ${mvs}")
    val moves = mvs.grouped(3).map(MoveBuilderSfen.parseUsenString)

    // make moves
    val b = moves.foldLeft[Branch](Branch(initialStateFunc(offset), offset)) { (br, m) =>
      br.makeMove(m).getOrElse(throw new RecordFormatException(1, s"Invalid move: ${m.toUsenString}"))
    }

    // set final action
    val finalAction = fa.nonEmpty.option(SpecialMove.parseUsenString(fa, b.lastState, b.lastMoveTo))
    b.updateFinalAction(finalAction)
  }

}

trait SfenGameReader {
  def parseSfenString(s: String)(implicit stateCache: StateCache): Game = Game(Branch.parseSfenString(s))

  def parseUsenString(s: String)(implicit stateCache: StateCache): Game = {
    val tokens = s.split("~")
    if (tokens.length < 2) throw new RecordFormatException(1, s"game description must have at least two sections: ${s}")

    val initialState = State.parseUsenString(tokens(0))
    val trunk = Branch.parseUsenString(tokens(1), initialState)
    val branches = tokens.drop(2).map(ss => Branch.parseUsenString(ss, trunk)).toVector
    Game(trunk, branches)
  }
}

trait SfenBranchWriter {
  def initialState: State

  def offset: Int

  def moves: Vector[Move]

  def finalAction: Option[SpecialMove]

  def comments: Map[Int, String]

  /**
    * Make Sfen string
    *
    * @return "{initialState} {offset} [{move}...]"
    */
  def toSfenString: String =
    (Seq(initialState.toSfenString, offset.toString) ++ moves.map(_.toSfenString)).mkString(" ")

  /**
    * Make Usen string
    *
    * @return "{offset}.[{move}...].[{final}]"
    */
  def toUsenString: String = {
    Seq(offset.toString, moves.map(_.toUsenString).mkString, finalAction.map(_.toUsenString).getOrElse("")).mkString(".")
  }
}

trait SfenGameWriter extends SfenLike with UsenLike {
  def trunk: Branch

  def branches: Vector[Branch]

  /**
    * Make trunk Sfen string
    *
    * @return "{board} {turn} {hand} {offset} [{move}...]"
    */
  override def toSfenString: String = trunk.toSfenString

  override def toUsenString: String = (Seq(trunk.initialState.toUsenString, trunk.toUsenString) ++ branches.map(_.toUsenString)).mkString("~")
}