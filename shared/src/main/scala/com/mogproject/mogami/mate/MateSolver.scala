package com.mogproject.mogami.mate

import com.mogproject.mogami._
import com.mogproject.mogami.core.state.StateCache

import scala.annotation.tailrec
import scala.collection.mutable

/**
  * Solve a mate problem.
  */
object MateSolver {

  val calculated: mutable.Map[StateHash, StateHash] = mutable.Map.empty

  def solve(state: State, lastMoveTo: Option[Square] = None, maxDepth: Int = 9, timeLimitMillis: Long = 10000)(implicit stateCache: StateCache): Option[Seq[Move]] = {
    3 to maxDepth by 2 map { n =>
      val startTime = System.currentTimeMillis()
      val ret = depthFirstSearch(state, n)
      val endTime = System.currentTimeMillis()

      ret match {
        case Some(result) =>
          val states = result.flatMap(stateCache.get)
          type T = (State, Option[Square], Option[Move])
          val moves = states.scanLeft[T, List[T]]((state, lastMoveTo, None)) { case ((st, lmt, mv), nst) =>
            val m = st.createMoveFromNextState(nst, lmt)
            (nst, m.map(_.to), m)
          }
          return if (moves.isEmpty) Some(Seq.empty) else Some(moves.tail.map(_._3.get))
        case None if endTime - startTime > timeLimitMillis =>
          return None // time up
        case None => // continue searching
      }
    }
    None // depth limit exceeded
  }

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

  def depthFirstSearch(initialState: State, maxDepth: Int)(implicit stateCache: StateCache): Option[List[StateHash]] = {

    def f(sofar: List[List[StateHash]], solution: List[StateHash], isUnProven: Boolean): Option[List[StateHash]] = {
      val depth = sofar.length

      if (sofar.isEmpty) {
        if (solution.isEmpty) if (isUnProven) None else Some(Nil) else Some(solution.reverse.tail)
      } else {
        // get the current state
        stateCache.apply(sofar.head.head) match {
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
                  f(removeVerifiedThis(sofar), if (solution.isEmpty) stateCache.set(s) :: sofar.map(_.head) else solution, isUnProven)
                case None =>
                  if (checkMoves.isEmpty) {
                    f(removeVerified(sofar), Nil, isUnProven = false) // no solution
                  } else {
                    f(checkMoves.toList.flatMap(st.makeMove).map(stateCache.set) :: sofar, solution, isUnProven)
                  }
              }
            }

          case st =>
            //
            // defender's turn
            //
            val legalMoves = st.legalMoves(None).toList

            if (legalMoves.isEmpty) {
              if (stateCache.get(sofar.tail.head.head).get.createMoveFromNextState(st).get.isPawnDrop) {
                f(removeParentNode(sofar), Nil, isUnProven) // Uchifuzume
              } else {
                f(removeVerified(sofar), if (solution.isEmpty) sofar.map(_.head) else solution, isUnProven)
              }
            } else {
              f(legalMoves.flatMap(st.makeMove).map(stateCache.set) :: sofar, solution, isUnProven)
            }
        }
      }
    }

    f(List(List(stateCache.set(initialState))), Nil, isUnProven = false)
  }

  def findImmediateCheckmate(state: State, checkMoves: Seq[Move]): Option[State] = {
    val mvs = checkMoves.filter(mv => !mv.isPawnDrop)
    if (mvs.isEmpty) None else mvs.view.map(mv => state.makeMove(mv).get).find(_.isMated)
  }
}
