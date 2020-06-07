package com.mogproject.mogami.core.game

import com.mogproject.mogami.core.game.Game.{BranchNo, CommentType, GamePosition, HistoryHash}
import com.mogproject.mogami.core.state.{State, StateCache}
import com.mogproject.mogami.core.io._
import com.mogproject.mogami.core.move._
import com.mogproject.mogami.core.state.StateHash.StateHash
import com.mogproject.mogami.util.Implicits._

import scala.util.Try

/**
  * Game
  */
case class Game(trunk: Branch,
                branches: Vector[Branch] = Vector.empty,
                gameInfo: GameInfo = GameInfo(),
                comments: CommentType = Map.empty
               )(implicit val stateCache: StateCache) extends CsaGameWriter with SfenGameWriter with KifGameWriter {

  type ForkList = Map[HistoryHash, Map[Move, BranchNo]]

  override def equals(other: Any): Boolean = other match {
    case that: Game => this.trunk == that.trunk && this.branches == that.branches && this.gameInfo == that.gameInfo && this.comments == that.comments
    case _ => false
  }

  def copy(newTrunk: Branch = this.trunk,
           newBranches: Vector[Branch] = this.branches,
           newGameInfo: GameInfo = this.gameInfo,
           newComments: CommentType = this.comments) = Game(newTrunk, newBranches, newGameInfo, newComments)

  //
  // helper functions
  //
  def getBranch(branchNo: BranchNo): Option[Branch] = (branchNo == 0).fold(Some(trunk), branches.get(branchNo - 1))

  def withBranch[A](branchNo: BranchNo)(f: Branch => A): Option[A] = getBranch(branchNo).map(f)

  private[this] def withGamePosition[A](gamePosition: GamePosition)(f: (Branch, Int) => Option[A]): Option[A] = if (gamePosition.isTrunk) {
    f(trunk, gamePosition.position)
  } else {
    getBranch(gamePosition.branch).flatMap { br =>
      f((gamePosition.position < br.offset).fold(trunk, br), gamePosition.position)
    }
  }

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

  def getMove(gamePosition: GamePosition): Option[Move] = withGamePosition(gamePosition)(_.getMove(_))

  /**
    * Get the set of all state hashes being used for this Game instance
    *
    * @return set of state hashes (not history hashes)
    */
  def getStateHashSet: Set[StateHash] = (trunk +: branches).flatMap(_.history).toSet

  //
  // Comments
  //
  def hasComment(gamePosition: GamePosition): Boolean = getHistoryHash(gamePosition).exists(comments.contains)

  def getComment(gamePosition: GamePosition): Option[String] = getHistoryHash(gamePosition).flatMap(comments.get)

  def updateComment(gamePosition: GamePosition, comment: String): Option[Game] = {
    if (comment.isEmpty)
      clearComment(gamePosition)
    else {
      getHistoryHash(gamePosition).map(h => this.copy(newComments = comments.updated(h, comment)))
    }
  }

  def clearComment(gamePosition: GamePosition): Option[Game] = {
    getHistoryHash(gamePosition).map(h => this.copy(newComments = comments - h))
  }

  //
  // Forks
  //
  private[this] lazy val forkList: ForkList = {
    ((trunk, -1) +: branches.zipWithIndex).foldLeft(Map.empty[HistoryHash, Map[Move, BranchNo]]) { case (m, (b, i)) =>
      val branchNo = i + 1
      b.getNextMoveList.foldLeft(m) { case (mm, (h, mv)) =>
        mm.get(h) match {
          case Some(ss) if ss.contains(mv) => mm // existing move
          case Some(ss) => mm.updated(h, ss.updated(mv, branchNo)) // existing hash, new move
          case None => mm.updated(h, Map(mv -> branchNo)) // new hash
        }
      }
    }.filter(_._2.size > 1)
  }

  def getForks(gamePosition: GamePosition): Vector[(Move, BranchNo)] = if (hasFork(gamePosition)) {
    withBranch(gamePosition.branch) { br =>
      (for {
        h <- getHistoryHash(gamePosition).toVector
        mm <- forkList.get(h).toVector
        (m, b) <- mm
        mv <- getMove(gamePosition)
        if (gamePosition.position < br.offset).fold(0, gamePosition.branch) != b
        if m != mv
      } yield (m, b)).sortBy(_._2)
    }.getOrElse(Vector.empty)
  } else Vector.empty

  def hasFork(gamePosition: GamePosition): Boolean = getHistoryHash(gamePosition).exists(forkList.contains)

  /**
    * Create a new branch
    *
    * This is eligible when
    *   - the position is not the last
    *   - the position is on the trunk --or-- the move is new
    */
  def createBranch(gamePosition: GamePosition, move: Move, isFreeMode: Boolean = false): Option[Game] = withBranch(gamePosition.branch) { br =>
    val moveOnThisBranch = (gamePosition.position < br.offset).fold(trunk, br).getMove(gamePosition.position)

    val ok = moveOnThisBranch.exists(_ != move) && (gamePosition.isTrunk || getForks(gamePosition).forall(_._1 != move))

    if (ok) {
      (if (gamePosition.isTrunk || gamePosition.position < br.offset) {
        Try(Branch(
          trunk.history(gamePosition.position - trunk.offset),
          gamePosition.position,
          Vector(move),
          None,
          getHistoryHash(gamePosition),
          isFreeMode = isFreeMode
        )).toOption
      } else {
        val diff = gamePosition.position - br.offset
        Branch(
          trunk.history(br.offset - trunk.offset),
          br.offset, br.moves.take(diff),
          hint = Some(BranchHint(br.history.take(diff + 1), br.historyHash.take(diff + 1))),
          isFreeMode = isFreeMode
        ).makeMove(move)
      }) map { newBranch =>
        copy(newBranches = branches :+ newBranch)
      }
    } else {
      // the position is the last position of the current branch, or the fork already exists
      None
    }
  }.flatten

  def deleteBranch(branchNo: BranchNo): Option[Game] = if (branchNo == 0 || !branches.isDefinedAt(branchNo - 1)) {
    None // trunk cannot be deleted
  } else {
    Some(copy(newBranches = branches.patch(branchNo - 1, Nil, 1)))
  }

  def updateBranch(branchNo: BranchNo)(f: Branch => Option[Branch]): Option[Game] = {
    if (branchNo == 0) {
      f(trunk).map(tr => this.copy(newTrunk = tr))
    } else {
      for {
        br <- getBranch(branchNo)
        index = branchNo - 1
        nxt <- f(br)
      } yield {
        this.copy(newBranches = branches.updated(index, nxt))
      }
    }
  }

  //
  //
  //
  def getFinalAction(branchNo: BranchNo): Option[SpecialMove] = withBranch(branchNo)(_.finalAction).flatten

  /**
    * Create a truncated game at a specific position
    */
  def truncated(gamePosition: GamePosition): Game = {
    def f(t: Branch, bs: Seq[Branch]): CommentType = {
      val validKeys = (t +: bs).foldLeft(Set.empty[HistoryHash])(_ ++ _.historyHash)
      comments.view.filterKeys(validKeys).toMap // @note .toMap is required with Scala 2.13
    }

    if (gamePosition.isTrunk) {
      // delete branches if needed
      val newTrunk = trunk.truncated(gamePosition.position)
      val newBranches = branches.filter(_.offset <= gamePosition.position)
      copy(newTrunk = newTrunk, newBranches = newBranches, newComments = f(newTrunk, newBranches))
    } else {
      withBranch(gamePosition.branch) { br =>
        val newBranches = branches.updated(gamePosition.branchIndex, br.truncated(gamePosition.position))
        copy(newBranches = newBranches, newComments = f(trunk, newBranches))
      }.getOrElse(this)
    }
  }


  /**
    * Drop all elapsed-time information
    *
    * @return new Game without elapsed-time information
    */
  def dropElapsedTime: Game = copy(newTrunk = trunk.dropElapsedTime, newBranches = branches.map(_.dropElapsedTime))
}

object Game extends CsaGameReader with SfenGameReader with KifGameReader {
  def apply()(implicit stateCache: StateCache): Game = new Game(Branch())

  def apply(isFreeMode: Boolean)(implicit stateCache: StateCache): Game = new Game(Branch(isFreeMode))

  type BranchNo = Int // branch number: root = 0

  type Position = Int // regarding offset

  // workaround for IntelliJ IDEA
  override def parseSfenString(s: String, isFreeMode: Boolean)(implicit stateCache: StateCache): Game = super.parseSfenString(s, isFreeMode)

  override def parseUsenString(s: String, isFreeMode: Boolean)(implicit stateCache: StateCache): Game = super.parseUsenString(s, isFreeMode)

  override def parseKifString(nel: NonEmptyLines, isFreeMode: Boolean)(implicit stateCache: StateCache): Game = super.parseKifString(nel, isFreeMode)

  override def parseKi2String(nel: NonEmptyLines, isFreeMode: Boolean)(implicit stateCache: StateCache): Game = super.parseKi2String(nel, isFreeMode)

  override def parseCsaString(nel: NonEmptyLines, isFreeMode: Boolean)(implicit stateCache: StateCache): Game = super.parseCsaString(nel, isFreeMode)

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
