package com.mogproject.mogami.core.game

import com.mogproject.mogami.core.game.Game.{BranchNo, CommentType, GamePosition, HistoryHash}
import com.mogproject.mogami.core.state.StateCache.Implicits._
import com.mogproject.mogami.core.state.{State, StateCache}
import com.mogproject.mogami.core.state.StateHash.StateHash
import com.mogproject.mogami.core.io._
import com.mogproject.mogami.core.move._
import com.mogproject.mogami.util.Implicits._

import scala.collection.mutable

/**
  * Game
  */
case class Game(trunk: Branch = Branch(),
                branches: Vector[Branch] = Vector.empty,
                gameInfo: GameInfo = GameInfo(),
                comments: CommentType = Map.empty
               )(implicit val stateCache: StateCache) extends CsaGameWriter with SfenGameWriter with KifGameWriter {

  type ForkList = Map[Int, Vector[(Move, BranchNo)]]
  private[this] val forkList: mutable.Map[BranchNo, ForkList] = mutable.Map.empty

  //
  // helper functions
  //
  def getBranch(branchNo: BranchNo): Option[Branch] = (branchNo == 0).fold(Some(trunk), branches.get(branchNo - 1))

  def withBranch[A](branchNo: BranchNo)(f: Branch => A): Option[A] = getBranch(branchNo).map(f)

  private[this] def withGamePosition[A](gamePosition: GamePosition)(f: (Branch, Int) => Option[A]): Option[A] = if (gamePosition.isTrunk) {
    f(trunk, gamePosition.position)
  } else {
    getBranch(gamePosition.branch).flatMap { br =>
      f((gamePosition.position <= br.offset).fold(trunk, br), gamePosition.position)
    }
  }

  def createBranch(gamePosition: GamePosition, move: Move): Option[Game] = withBranch(gamePosition.branch) { br =>
    val moveOnThisBranch = (gamePosition.position < br.offset).fold(trunk, br).getMove(gamePosition.position)
    val forks = getForkList(gamePosition.branch)

    val ok = moveOnThisBranch.exists(_ != move) && forks.get(gamePosition.position).forall(_.forall(_._1 != move))

    if (ok) {
      (if (gamePosition.isTrunk || gamePosition.position < br.offset) {
        Some(Branch(trunk.history(gamePosition.position - trunk.offset), gamePosition.position, Vector(move)))
      } else {
        val diff = gamePosition.position - br.offset
        Branch(trunk.history(br.offset - trunk.offset), br.offset, br.moves.take(diff),
          hint = Some(BranchHint(br.history.take(diff + 1), br.historyHash.take(diff + 1)))).makeMove(move)
      }) map { newBranch =>
        copy(branches = branches :+ newBranch)
      }
    } else {
      // the position is the last position of the current branch, or the fork already exists
      None
    }
  }.flatten

  def deleteBranch(branchNo: BranchNo): Option[Game] = if (branchNo == 0 || !branches.isDefinedAt(branchNo - 1)) {
    None // trunk cannot be deleted
  } else {
    Some(copy(branches = branches.patch(branchNo - 1, Nil, 1)))
  }

  def updateBranch(branchNo: BranchNo)(f: Branch => Option[Branch]): Option[Game] = {
    if (branchNo == 0) {
      f(trunk).map(tr => this.copy(trunk = tr))
    } else {
      for {
        br <- getBranch(branchNo)
        index = branchNo - 1
        nxt <- f(br)
      } yield {
        this.copy(branches = branches.updated(index, nxt))
      }
    }
  }

  /**
    * @param branchNo map of offset -> {vector of (move, branch number)
    * @return
    */
  protected[game] def getForkList(branchNo: BranchNo): ForkList = forkList.getOrElse(branchNo, {
    // update stored value
    val ls = createForkList(branchNo)
    forkList.update(branchNo, ls)
    ls
  })

  protected[game] def createForkList(branchNo: BranchNo): ForkList = {
    val m: Map[(Int, Move), BranchNo] = if (branchNo == 0) {
      findForksOnTrunk(trunk.offset + trunk.moves.length)
    } else {
      withBranch(branchNo) { br =>
        // find parent == trunk
        val preceding = findForksOnTrunk(br.offset)

        // add trunk as a fork
        val trunkFork = if (trunk.moves.isDefinedAt(br.offset)) Map((br.offset, trunk.moves(br.offset)) -> 0) else Map.empty

        // find brother nodes
        val brothers = branches.zipWithIndex.filter { case (b, i) => i != branchNo - 1 && b.offset == br.offset }
        preceding ++ trunkFork ++ findForksOnBrotherNodes(br.history, brothers)
      }.getOrElse(Map.empty)
    }

    m.groupBy(_._1._1).map { case (k, v) => k -> v.map { case ((_, mv), br) => (mv, br) }.toSeq.sortBy(_._2).toVector }
  }

  private[this] def findForksOnTrunk(offsetLimit: Int): Map[(Int, Move), BranchNo] = {
    branches.zipWithIndex.foldLeft(Map.empty[(Int, Move), BranchNo]) { case (sofar, (br, i)) =>
      (br.offset, br.moves.headOption) match {
        case (os, Some(mv)) if os < offsetLimit && !sofar.contains((os, mv)) => sofar.updated((os, mv), i + 1)
        case _ => sofar // already set the same offset/move pair or no moves
      }
    }
  }

  private[this] def findForksOnBrotherNodes(baseHistory: Vector[StateHash], brothers: Vector[(Branch, Int)]): Map[(Int, Move), BranchNo] = {
    brothers.foldLeft(Map.empty[(Int, Move), BranchNo]) { case (sofar, (br, i)) =>
      baseHistory.zip(br.history).tail.indexWhere { case (a, b) => a != b } match {
        case -1 =>
          println(s"Error: Identical branch: ${i}")
          sofar
        case index if !sofar.contains((br.offset + index, br.moves(index))) => sofar.updated((br.offset + index, br.moves(index)), i + 1)
        case _ => sofar // already set the same offset/move
      }
    }
  }

  def getForks(gamePosition: GamePosition): Vector[(Move, BranchNo)] = getForkList(gamePosition.branch).getOrElse(gamePosition.position, Vector.empty)

  /**
    * Get all moves from the trunk's start position
    *
    * @param branchNo branch number (trunk:0)
    */
  def getAllMoves(branchNo: BranchNo): Vector[Move] = if (branchNo == 0) {
    trunk.moves
  } else {
    getBranch(branchNo).map { br => trunk.moves.take(br.offset - trunk.offset) ++ br.moves }.getOrElse(Vector.empty)
  }

  /**
    * Get the state at a specific game position
    *
    * @param gamePosition game position
    * @return None if the game position is invalid
    */
  def getState(gamePosition: GamePosition): Option[State] = withGamePosition(gamePosition)(_.getState(_))

  def getHistoryHash(gamePosition: GamePosition): Option[HistoryHash] =
    withGamePosition(gamePosition)((br, pos) => br.historyHash.get(pos - br.offset))

  //
  // Comments
  //
  def hasComment(gamePosition: GamePosition): Boolean = getHistoryHash(gamePosition).exists(comments.contains)

  def getComment(gamePosition: GamePosition): Option[String] = getHistoryHash(gamePosition).flatMap(comments.get)

  def updateComment(gamePosition: GamePosition, comment: String): Option[Game] = {
    if (comment.isEmpty)
      clearComment(gamePosition)
    else {
      getHistoryHash(gamePosition).map(h => this.copy(comments = comments.updated(h, comment)))
    }
  }

  def clearComment(gamePosition: GamePosition): Option[Game] = {
    getHistoryHash(gamePosition).map(h => this.copy(comments = comments - h))
  }

  def getFinalAction(branchNo: BranchNo): Option[SpecialMove] = withBranch(branchNo)(_.finalAction).flatten

  def hasFork(gamePosition: GamePosition): Boolean = getForkList(gamePosition.branch).keySet.contains(gamePosition.position)

  /**
    * Create a truncated game at a specific position
    */
  def truncated(gamePosition: GamePosition): Game = {
    def f(t: Branch, bs: Seq[Branch]): CommentType = {
      val validKeys = (t +: bs).foldLeft(Set.empty[HistoryHash])(_ ++ _.historyHash)
      comments.filterKeys(validKeys)
    }

    if (gamePosition.isTrunk) {
      // delete branches if needed
      val newTrunk = trunk.truncated(gamePosition.position)
      val newBranches = branches.filter(_.offset <= gamePosition.position)
      copy(trunk = newTrunk, branches = newBranches, comments = f(newTrunk, newBranches))
    } else {
      withBranch(gamePosition.branch) { br =>
        val newBranches = branches.updated(gamePosition.branchIndex, br.truncated(gamePosition.position))
        copy(branches = newBranches, comments = f(trunk, newBranches))
      }.getOrElse(this)
    }
  }
}

object Game extends CsaGameReader with SfenGameReader with KifGameReader {

  type BranchNo = Int // branch number: root = 0

  type Position = Int // regarding offset

  case class GamePosition(branch: BranchNo, position: Position) {
    require(branch >= 0, "branch must not be negative")
    require(position >= 0, "position must not be negative")

    def isTrunk: Boolean = branch == 0

    def branchIndex: Int = branch - 1
  }

  /**
    * A hash value of a sequence of moves. This differenciates the same state with different histories.
    */
  type HistoryHash = Long

  type CommentType = Map[HistoryHash, String]
}
