package com.mogproject.mogami.core.move

import com.mogproject.mogami._
import com.mogproject.mogami.core.move.Movement._
import com.mogproject.mogami.util.Implicits._

/**
  * Move builder for CSA-formatted string
  */
trait MoveBuilder {
  protected def isCheckMove(state: State, from: Option[Square], to: Square, newPtype: Ptype): Boolean = {
    state.getKing(!state.turn).exists { k =>
      val occ = state.occupancy.set(to)
      val newOccAll = from.map(occ.reset).getOrElse(occ)
      val directAttack = Attack.get(Piece(state.turn, newPtype), Some(to), newOccAll, BitBoard.empty).get(k)
      val rangedAttack = from.exists { fr =>
        state.board.exists {
          case (s, p) if p.owner == state.turn && p.isRanged && fr != s => Attack.get(p, Some(s), newOccAll, BitBoard.empty).get(k)
          case _ => false
        }
      }
      directAttack || rangedAttack
    }
  }

  /**
    * Get set of squares from which the same ptype can move to the same destinatiton.
    */
  protected def getAmbiguousPieces(state: State, from: Option[Square], to: Square, oldPtype: Ptype): Set[Square] = {
    state.board
      .filter(_._2 == Piece(state.turn, oldPtype))
      .keySet.filter(fr => !from.contains(fr) && state.attackBBOnBoard(state.turn)(fr).get(to))
  }

  protected def getRelation(player: Player, from: Square, to: Square): (Int, Int) = {
    val r = from.rank.compare(to.rank)
    val f = from.file.compare(to.file)
    player.isBlack.fold((r, f), (-r, -f))
  }

  protected def getMovement(state: State, from: Option[Square], to: Square, oldPtype: Ptype): Option[Movement] = {
    val ambs = getAmbiguousPieces(state, from, to, oldPtype)

    from match {
      case _ if ambs.isEmpty => None
      case None => Some(Dropped)
      case Some(fr) =>
        val rel = getRelation(state.turn, fr, to)
        val relations = ambs.map(getRelation(state.turn, _, to))
        if (!relations.exists(_._1 == rel._1)) rel._1 match {
          case 0 => Some(Horizontally)
          case 1 => Some(Upward)
          case -1 => Some(Downward)
          // $COVERAGE-OFF$
          case _ => throw new RuntimeException(s"Unexpected movement: state=${state}, from=${from}, to=${to}, oldPtype=${oldPtype}")
          // $COVERAGE-ON$
        } else if (!relations.exists(_._2 == rel._2)) rel._2 match {
          case 0 if !List(BISHOP, ROOK).contains(oldPtype.demoted) => Some(Vertical)
          case 0 if relations.head._2 == -1 => Some(Rightwards) // relations must contain one another
          case 0 if relations.head._2 == 1 => Some(Leftwards) // relations must contain one another
          case 1 => Some(Rightwards)
          case -1 => Some(Leftwards)
          // $COVERAGE-OFF$
          case _ => throw new RuntimeException(s"Unexpected movement: state=${state}, from=${from}, to=${to}, oldPtype=${oldPtype}")
          // $COVERAGE-ON$
        } else rel match {
          case (-1, -1) => Some(LeftDownward)
          case (-1, 1) => Some(RightDownward)
          case (0, -1) => Some(LeftHorizontally)
          case (0, 1) => Some(RightHorizontally)
          case (1, -1) => Some(LeftUpward)
          case (1, 1) => Some(RightUpward)
          case (_, 0) => Some(Vertical)
          // $COVERAGE-OFF$
          case _ => throw new RuntimeException(s"Unexpected movement: state=${state}, from=${from}, to=${to}, oldPtype=${oldPtype}")
          // $COVERAGE-ON$
        }
    }

  }

  def toMove(state: State, lastMoveTo: Option[Square] = None, isStrict: Boolean = true): Option[Move]
}

