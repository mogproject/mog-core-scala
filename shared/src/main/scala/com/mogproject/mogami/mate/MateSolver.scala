package com.mogproject.mogami.mate

import com.mogproject.mogami._
import com.mogproject.mogami.core.state.ThreadUnsafeStateCache

import scala.annotation.tailrec

/**
  * Solve a mate problem.
  */
object MateSolver {
  implicit lazy val mateSolverStateCache = new ThreadUnsafeStateCache()

  // statistics
  private[this] var numComputedNodes: Int = 0
  private[this] var timeoutCounter: Int = 0
  private[this] var timeLimit: Long = 0

  def solve(state: State, lastMoveTo: Option[Square] = None, maxDepth: Int = 29, timeLimitMillis: Long = 15000): Option[Seq[Move]] = {
    // initialization
    numComputedNodes = 0
    timeoutCounter = 0
    timeLimit = System.currentTimeMillis() + timeLimitMillis

    solveImpl(state, lastMoveTo, maxDepth, timeLimitMillis)
  }

  private[this] def solveImpl(state: State, lastMoveTo: Option[Square], maxDepth: Int, timeLimitMillis: Long): Option[Seq[Move]] = {
    3 to maxDepth by 2 map { n =>
      depthFirstSearch(state, n) match {
        case Some(result) =>
          val states = result.flatMap(mateSolverStateCache.get)
          type T = (State, Option[Square], Option[Move])
          val moves = states.scanLeft[T, List[T]]((state, lastMoveTo, None)) { case ((st, lmt, mv), nst) =>
            val m = st.createMoveFromNextState(nst, lmt)
            (nst, m.map(_.to), m)
          }
          return if (moves.isEmpty) Some(Seq.empty) else Some(moves.tail.map(_._3.get))
        case None => // continue searching
      }
    }
    None // depth limit exceeded
  }

  private[this] def checkTimeout(): Boolean = (timeoutCounter & 2047) == 0 && System.currentTimeMillis() > timeLimit

  private[this] def refreshStateCache(keep: => Set[StateHash]): Unit =
    if ((timeoutCounter & 2047) == 0 && mateSolverStateCache.numKeys > 100000) mateSolverStateCache.refresh(keep)

  protected[mate] def removeParentNode(xss: List[List[StateHash]]): List[List[StateHash]] = if (xss.isEmpty) Nil else removeLeaf(xss.tail)

  @tailrec
  protected[mate] def removeVerified(xss: List[List[StateHash]]): List[List[StateHash]] =
    if (xss.isDefinedAt(1) && xss(1).length == 1) removeVerified(xss.drop(2)) else removeParentNode(xss)

  protected[mate] def removeVerifiedThis(xss: List[List[StateHash]]): List[List[StateHash]] =
    if (xss.headOption.exists(_.length == 1)) removeVerified(xss.tail) else removeLeaf(xss)

  @tailrec
  final protected[mate] def removeLeaf(xss: List[List[StateHash]]): List[List[StateHash]] =
    if (xss.isEmpty)
      Nil
    else if (xss.head.isEmpty || xss.head.tail.isEmpty)
      removeLeaf(xss.tail)
    else
      xss.head.tail :: xss.tail

  def depthFirstSearch(initialState: State, maxDepth: Int): Option[List[StateHash]] = {

    def f(sofar: List[List[StateHash]], solution: List[StateHash], isUnProven: Boolean): Option[List[StateHash]] = {
      timeoutCounter += 1

      if (checkTimeout()) {
        None
      } else {
        refreshStateCache(sofar.flatten.toSet ++ solution)

        val depth = sofar.length

        if (sofar.isEmpty) {
          if (solution.isEmpty) if (isUnProven) None else Some(Nil) else Some(solution.reverse.tail)
        } else {
          // get the current state
          mateSolverStateCache.apply(sofar.head.head) match {
            case st if depth % 2 == 1 =>
              //
              // attacker's turn
              //
              if (depth > maxDepth) {
                f(removeVerified(sofar), Nil, isUnProven = true)
              } else {
                val checkMoves = st.legalMoves(None).filter(_.isCheck)

                findImmediateCheckmate(st, checkMoves) match {
                  case Some(s) => // found an immediate checkmate
                    f(removeVerifiedThis(sofar), if (solution.isEmpty) mateSolverStateCache.set(s) :: sofar.map(_.head) else solution, isUnProven)
                  case None =>
                    if (checkMoves.isEmpty) {
                      f(removeVerified(sofar), Nil, isUnProven = isUnProven) // no solution
                    } else {
                      // println(s"at ${depth}: " + sortMoves(checkMoves).map(_.toJapaneseNotationString))
                      // numComputedNodes += checkMoves.length
                      f(sortMoves(checkMoves).toList.map(mv => makeMove(st, mv)) :: sofar, solution, isUnProven)
                    }
                }
              }

            case st =>
              //
              // defender's turn
              //
              val legalMoves = st.legalMoves(None)

              if (legalMoves.isEmpty) {
                if (mateSolverStateCache.get(sofar.tail.head.head).get.createMoveFromNextState(st).get.isPawnDrop) {
                  f(removeParentNode(sofar), Nil, isUnProven) // Uchifuzume
                } else {
                  f(removeVerified(sofar), if (solution.isEmpty) sofar.map(_.head) else solution, isUnProven)
                }
              } else {
                // println(s"df ${depth}: " + sortMoves(legalMoves).map(_.toJapaneseNotationString))
                // numComputedNodes += legalMoves.length
                f(sortMoves(legalMoves).toList.map(mv => makeMove(st, mv)) :: sofar, solution, isUnProven)
              }
          }
        }
      }
    }

    f(List(List(mateSolverStateCache.set(initialState))), Nil, isUnProven = false)
  }

  def findImmediateCheckmate(state: State, checkMoves: Seq[Move]): Option[State] = {
    val mvs = checkMoves.filter(mv => !mv.isPawnDrop)
    if (mvs.isEmpty) None else mvs.view.map(mv => mateSolverStateCache.get(makeMove(state, mv)).get).find(_.isMated)
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
}
