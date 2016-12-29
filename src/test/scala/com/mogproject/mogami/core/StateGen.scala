package com.mogproject.mogami.core

import org.scalacheck.Gen

import scala.annotation.tailrec
import com.mogproject.mogami._
import com.mogproject.mogami.core.PieceConstant._
import com.mogproject.mogami.util.MapUtil

import scala.util.Try


object StateGen {

  private[this] def boardCandidates(piece: Piece, board: Map[Square, Piece]): Set[Square] = {
    // Two pawns in the same file is prohibited.
    val illegalFiles: Set[Int] = piece.ptype match {
      case PAWN => board.withFilter(_._2 == piece).map(_._1.file).toSet
      case _ => Set.empty
    }

    val illegalRanks: Set[Int] = piece match {
      case BP | BL => Set(1)
      case BN => Set(1, 2)
      case WP | WL => Set(9)
      case WN => Set(8, 9)
      case _ => Set.empty
    }

    val posOcc = board.keys.toSet
    (SquareGen.boards.toSet -- posOcc).filterNot(p => illegalFiles.contains(p.file) || illegalRanks.contains(p.rank))
  }

  @tailrec
  private[this] def f(pts: List[Ptype], boardSofar: BoardType, handSofar: HandType): (BoardType, HandType) = pts match {
    case pt :: xs =>
      val kingPlaced: Set[Player] = boardSofar.values.withFilter(_.ptype == KING).map(_.owner).toSet
      val turnCandidates: Set[Player] = Player.constructor.toSet -- (if (pt == KING) kingPlaced else Set.empty)
      val handCandidates: Set[Square] = if (Ptype.inHand.contains(pt)) Set(Square.HAND) else Set.empty

      val (newBoard, newHands) = (for {
        t <- Gen.oneOf(turnCandidates.toSeq)
        p <- Gen.oneOf(pt, pt, if (pt.canPromote) pt.promoted else pt) // promoted rate = 1/3
        pos <- Gen.oneOf(boardCandidates(Piece(t, p), boardSofar).toSeq ++ handCandidates.toSeq.flatMap(Seq.fill(10)(_)))
      } yield {
        if (pos == Square.HAND) {
          (boardSofar, MapUtil.incrementMap(handSofar, Piece(t, pt)))
        } else {
          (boardSofar + (pos -> Piece(t, p)), handSofar)
        }
      }).sample.get
      f(xs, newBoard, newHands)
    case Nil => (boardSofar, handSofar)
  }

  @tailrec
  private[this] def g(turn: Player, pts: List[Ptype], depth: Int): State = {
    if (depth >= 5) {
      State.HIRATE
    } else {
      val (board, hands) = f(pts, Map.empty, State.EMPTY_HANDS)
      val s = Try(State(turn, board, hands))  // test if the state is valid
      if (s.isSuccess)
        s.get
      else
        g(turn, pts, depth + 1)
    }
  }

  // State generator for scalacheck
  val statesWithFullPieces: Gen[State] = {
    val pts = State.capacity.flatMap { case (p, i) => List.fill(i)(p) }.toList

    for {
      t <- PlayerGen.players
    } yield {
      g(t, pts, 0)
    }
  }
}

