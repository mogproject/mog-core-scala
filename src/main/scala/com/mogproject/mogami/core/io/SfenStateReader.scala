package com.mogproject.mogami.core.io

import scala.annotation.tailrec
import com.mogproject.mogami.core.{Piece, Player, Square, State}
import com.mogproject.mogami.core.State.{BoardType, HandType}
import com.mogproject.mogami.util.BooleanOps.Implicits._

/**
  * Reads Sfen-formatted state
  */
trait SfenStateReader extends SfenFactory[State] {

  def parseSfenString(s: String): Option[State] = {
    for {
      _ <- Some({})
      tokens = s.split(" ", 3) if tokens.length == 3
      board <- parseBoard(tokens(0))
      turn <- Player.parseSfenString(tokens(1))
      hand <- parseHand(tokens(2))
    } yield State(turn, board, hand)
  }

  protected[io] def parseBoard(s: String): Option[BoardType] = {
    val patternEmpty = "^([1-9])(.*)".r
    val patternPiece = "^([+]?[A-Za-z])(.*)".r

    @tailrec
    def f(sofar: Option[BoardType], rest: String, file: Int, rank: Int): Option[BoardType] = (sofar, rest, file) match {
      case (Some(_), "", 0) => sofar
      case (Some(_), _, 0) => None
      case (Some(_), patternEmpty(n, r), _) => f(sofar, r, file - n.toInt, rank)
      case (Some(board), patternPiece(p, r), _) =>
        f(Piece.parseSfenString(p).map(pp => board.updated(Square(file, rank), pp)), r, file - 1, rank)
      case _ => None
    }

    @tailrec
    def g(sofar: Option[BoardType], tokens: List[String], rank: Int): Option[BoardType] = (sofar, tokens, rank) match {
      case (Some(_), Nil, 10) => sofar
      case (Some(_), x :: xs, n) if n <= 9 => g(f(sofar, x, 9, rank), xs, rank + 1)
      case _ => None
    }

    g(Some(Map.empty), s.split("/", 9).toList, 1)
  }

  protected[io] def parseHand(s: String): Option[HandType] = {
    val patternPiece = "^([0-9]*)([A-Za-z])(.*)".r

    @tailrec
    def f(sofar: Option[HandType], rest: String): Option[HandType] = (sofar, rest) match {
      case (Some(_), "") => sofar
      case (Some(hand), patternPiece(n, p, r)) =>
        f(Piece.parseSfenString(p).map(pp => hand.updated(pp, n.isEmpty.fold(1, n.toInt))), r)
      case _ => None
    }

    (s == "-").fold(Some(State.EMPTY_HANDS), f(Some(State.EMPTY_HANDS), s))
  }
}
