package com.mogproject.mogami.mate

import com.mogproject.mogami._
import com.mogproject.mogami.util.Implicits._
import com.mogproject.mogami.core.state.ThreadUnsafeStateCache

/**
  * Solve a mate problem.
  */
object MateSolver {
  implicit lazy val mateSolverStateCache = new ThreadUnsafeStateCache()

  // flags and statistics
  private[this] var timeout: Boolean = false
  private[this] var timeoutCounter: Int = 0
  private[this] var timeLimit: Long = 0

  def solve(state: State, lastMoveTo: Option[Square] = None, minDepth: Int = 3, maxDepth: Int = 29, timeLimitMillis: Long = 15000): Option[Seq[Move]] = {
    // initialization
    timeoutCounter = 0
    timeLimit = System.currentTimeMillis() + timeLimitMillis
    timeout = false

    solveImpl(state, lastMoveTo, minDepth, maxDepth, timeLimitMillis)
  }

  private[this] def solveImpl(state: State, lastMoveTo: Option[Square], minDepth: Int, maxDepth: Int, timeLimitMillis: Long): Option[Seq[Move]] = {
    minDepth to maxDepth by 2 map { depth =>
      searchAttack(state, depth - 1) match {
        case Some(xs) if xs.nonEmpty =>
          // Found a solution
          val moves = (lastMoveTo :: xs.init.map(mv => Some(mv.to))).zip(xs).map {
            case (Some(to), mv) if mv.to == to => mv.copy(isSameSquare = true)
            case (_, mv) => mv
          }
          return Some(moves)
        case Some(Nil) =>
          // No solutions
          return Some(Nil)
        case None => // No conclusion
      }
    }
    None // depth limit exceeded
  }

  private[this] def checkTimeout(): Boolean = (timeoutCounter & 1023) == 0 && System.currentTimeMillis() > timeLimit

  private[this] def refreshStateCache(keep: => Set[StateHash]): Unit =
    if ((timeoutCounter & 2047) == 0 && mateSolverStateCache.numKeys > 100000) mateSolverStateCache.refresh(keep)

  def findImmediateCheckmate(state: State, checkMoves: Seq[Move]): Option[Move] = {
    //    val mvs = checkMoves.filter(mv => !mv.isPawnDrop)
    //    if (mvs.isEmpty) None else mvs.view.map(mv => mateSolverStateCache.get(makeMove(state, mv)).get).find(_.isMated)
    for (mv <- checkMoves) {
      if (mateSolverStateCache.get(makeMove(state, mv)).get.isMated) return Some(mv)
    }
    None
  }

  private[this] def sortMoves(moves: Seq[Move]): Seq[Move] = {
    moves.sortBy(mv => (mv.captured.isEmpty, !mv.isDrop, !mv.promote))
  }

  /**
    * Get the next move's hash and register it to the cache
    */
  private[this] def makeMove(state: State, move: Move): StateHash = {
    val fromCache = StateHash.getNextStateHash(state, move)
    if (mateSolverStateCache.hasKey(fromCache)) fromCache else mateSolverStateCache.set(state.makeMove(move).get)
  }

  /**
    * Recursive functions
    */
  final private[this] def searchAttack(state: State, depth: Int): Option[List[Move]] = {
    timeoutCounter += 1

    if (timeout || checkTimeout()) {
      timeout = true // necessary for Javascript
      None
    } else {
      refreshStateCache(Set.empty)

      val checkMoves = state.legalMoves(None).filter(_.isCheck)
      val candidates = state.isUchifuzumePossible.fold(checkMoves.filterNot(_.isPawnDrop), checkMoves)

      if (candidates.isEmpty) {
        // No attack moves
        Some(Nil)
      } else {
        findImmediateCheckmate(state, candidates) match {
          case Some(mv) =>
            // Found an immediate checkmate
            Some(List(mv))
          case None if depth > 0 =>
            // Continue search.
            var sofar: Option[List[Move]] = Some(Nil)
            for (mv <- sortMoves(candidates)) {
              searchDefence(mateSolverStateCache.get(makeMove(state, mv)).get, depth - 1) match {
                case Some(Nil) => // No valid moves
                case Some(xs) if xs.nonEmpty => return Some(mv :: xs) // Found a solution
                case None => sofar = None // No conclusion
              }
            }
            sofar
          case _ =>
            // Reaches the max depth
            None
        }
      }
    }
  }

  final private[this] def searchDefence(state: State, depth: Int): Option[List[Move]] = {
    val legalMoves = state.legalMoves(None)
    var candidateLength = 0
    var candidate = List.empty[Move]

    for (mv <- sortMoves(legalMoves)) {
      searchAttack(mateSolverStateCache.get(makeMove(state, mv)).get, depth - 1) match {
        case None => return None // Reached the max depth
        case Some(xs) if xs.nonEmpty =>
          // Found a solution
          val len = xs.length
          if (len > candidateLength) {
            candidateLength = len
            candidate = mv :: xs
          }
        case Some(Nil) => return Some(Nil) // No solution
      }
    }
    Some(candidate)
  }
}
