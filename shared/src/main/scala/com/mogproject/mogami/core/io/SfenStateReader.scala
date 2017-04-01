package com.mogproject.mogami.core.io

import scala.annotation.tailrec
import com.mogproject.mogami._
import com.mogproject.mogami.util.Implicits._

/**
  * Reads Sfen-formatted state
  */
trait SfenStateReader extends SfenFactory[State] {

  def parseSfenString(s: String): State = {
    val tokens = s.split(" ", 3)
    if (tokens.length != 3) throw new RecordFormatException(1, s"there must be three tokens: ${s}")
    val board = parseBoard(tokens(0))
    val turn = Player.parseSfenString(tokens(1))
    val hand = parseHand(tokens(2))
    State(turn, board, hand)
  }

  protected[io] def parseBoard(s: String): BoardType = {
    val patternEmpty = "^([1-9])(.*)".r
    val patternPiece = "^([+]?[A-Za-z])(.*)".r

    @tailrec
    def f(sofar: BoardType, rest: String, file: Int, rank: Int): BoardType = (rest, file) match {
      case ("", 0) => sofar
      case (_, 0) => throw new RecordFormatException(1, s"too long rank expression (rank=${rank}): ${rest}")
      case (patternEmpty(n, r), _) => f(sofar, r, file - n.toInt, rank)
      case (patternPiece(p, r), _) => f(sofar.updated(Square(file, rank), Piece.parseSfenString(p)), r, file - 1, rank)
      case _ => throw new RecordFormatException(1, s"unexpected board expression: ${rest}")
    }

    @tailrec
    def g(sofar: BoardType, tokens: List[String], rank: Int): BoardType = (tokens, rank) match {
      case (Nil, 10) => sofar
      case (x :: xs, n) if n <= 9 => g(f(sofar, x, 9, rank), xs, rank + 1)
      case _ => throw new RecordFormatException(1, s"unexpected board format: ${s}")
    }

    g(Map.empty, s.split("/", 9).toList, 1)
  }

  protected[io] def parseHand(s: String): HandType = {
    val patternPiece = "^([0-9]*)([A-Za-z])(.*)".r

    @tailrec
    def f(sofar: HandType, rest: String): HandType = rest match {
      case "" => sofar
      case patternPiece(n, p, r) => f(sofar.updated(Hand(Piece.parseSfenString(p)), n.isEmpty.fold(1, n.toInt)), r)
      case _ => throw new RecordFormatException(1, s"unexpected hand expression: ${rest}")
    }

    (s == "-").fold(State.EMPTY_HANDS, f(State.EMPTY_HANDS, s))
  }
}
