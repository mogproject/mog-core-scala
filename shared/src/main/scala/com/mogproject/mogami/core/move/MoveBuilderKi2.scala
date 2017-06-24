package com.mogproject.mogami.core.move

import com.mogproject.mogami._
import com.mogproject.mogami.core.io._
import com.mogproject.mogami.core.io.kif.{Ki2Factory, Ki2Like}
import com.mogproject.mogami.core.move.Movement.{Dropped, Movement}
import com.mogproject.mogami.core.state.State
import com.mogproject.mogami.util.Implicits._

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

/**
  *
  */
case class MoveBuilderKi2(player: Player,
                          to: Option[Square],
                          oldPtype: Ptype,
                          movement: Option[Movement],
                          promote: Option[Boolean]) extends MoveBuilder with Ki2Like {

  /**
    * @param state state
    * @param moveTo move to
    * @return None if failed to find 'from'
    *         Some(None) from hand
    *         Some(Some(sq)) from board
    */
  protected[move] def findMoveFrom(state: State, moveTo: Square): Option[Option[Square]] = movement match {
    case None =>
      // calculate the from position from the state
      val boardCandidate = state.attackBBOnBoard(player).filter { case (sq, bb) => state.board.get(sq).exists(_.ptype == oldPtype) && bb.get(moveTo) }
      val canDrop = oldPtype.isHandType && state.attackBBInHand.get(Hand(player, oldPtype)).exists(_.get(moveTo))

      (boardCandidate.size, canDrop) match {
        case (0, true) => Some(None) // drop
        case (1, _) => Some(Some(boardCandidate.head._1)) // move
        case _ => None // ambiguous
      }
    case Some(Dropped) => Some(None)
    case Some(mvmt) =>
      state.attackBBOnBoard(player).find { case (sq, bb) =>
        state.board(sq).ptype == oldPtype && bb.get(moveTo) && getMovement(state, Some(sq), moveTo, oldPtype).exists(compareMovement(_, mvmt))
      }.map { case (sq, _) => Some(sq) }
  }

  private[this] def compareMovement(a: Movement, b: Movement): Boolean = {
    /** @note allows ambiguity between Upward and Vertical */
    a == b || a == Movement.Upward && b == Movement.Vertical
  }

  override def toMove(state: State, lastMoveTo: Option[Square] = None, isStrict: Boolean): Option[Move] = for {
    moveTo <- to match {
      case Some(t) => Some(t)
      case None => lastMoveTo
    }
    from <- findMoveFrom(state, moveTo)
    pr = promote.contains(true)
    isSame = to.isEmpty
    newPtype = pr.fold(oldPtype.promoted, oldPtype)
    captured = state.board.get(moveTo).map(_.ptype).filter(_ != KING)
    isCheck = isCheckMove(state, from, moveTo, newPtype)
  } yield {
    Move(player, from, moveTo, newPtype, pr, isSame, movement, captured, isCheck, None, isStrict)
  }

  override def toKi2String: String = Seq(
    player.toSymbolString(false),
    to.map(_.toKifString).getOrElse("同"),
    oldPtype.toKifString, // note: "龍" is used
    movement.map(_.kifString).getOrElse(""),
    promote.map(_.fold("成", "不成")).getOrElse("")
  ).mkString

}


object MoveBuilderKi2 extends Ki2Factory[MoveBuilderKi2] {

  private[this] def parseNotation(lineNo: LineNo, s: String): MoveBuilderKi2 = {

    def tryTwice[T](parser: NonEmptyLines => T)(s: String): (Int, T) = Try(parser(NonEmptyLines(lineNo, s.take(1)))) match {
      case Success(p) => (1, p)
      case Failure(_) => (2, parser(NonEmptyLines(lineNo, s.take(2))))
    }

    @tailrec
    def f(stage: Int = 0,
          rest: String,
          player: Player = BLACK,
          to: Option[Square] = None,
          oldPtype: Ptype = PAWN,
          movement: Option[Movement] = None,
          promote: Option[Boolean] = None): MoveBuilderKi2 = (stage, rest) match {
      case (n, "") if n >= 3 => MoveBuilderKi2(player, to, oldPtype, movement, promote)
      case (_, "") => throw new RecordFormatException(lineNo, s"incomplete move string: ${s}")
      case (0, _) => // evaluate player
        val pl = Player.constructor.find(p => rest.startsWith(p.toSymbolString(false))).getOrElse(
          throw new RecordFormatException(lineNo, s"invalid player expression: ${rest.head}")
        )
        f(1, rest.tail, pl, to, oldPtype, movement, promote)
      case (1, _) => //evaluate destination
        val (num, t) = if (rest.startsWith("同")) (1, None) else (2, Some(Square.parseKifString(NonEmptyLines(lineNo, rest.take(2)))))
        f(2, rest.drop(num), player, t, oldPtype, movement, promote)
      case (2, _) => // evaluate piece type
        val (num, p) = tryTwice(Ptype.parseKifString)(rest)
        f(3, rest.drop(num), player, to, p, movement, promote)
      case (3, _) => //evaluate promote
        val (num, pr) = Seq("不成" -> false, "生" -> false, "成" -> true) // 不成 must be prior to 成
          .find { case (k, _) => rest.endsWith(k) }
          .map[(Int, Option[Boolean])] { case (k, v) => (k.length, Some(v)) }
          .getOrElse((0, None))
        f(4, rest.dropRight(num), player, to, oldPtype, movement, pr)
      case (4, _) => // evaluate movement
        Movement.find(rest) match {
          case Some(m) => f(5, "", player, to, oldPtype, Some(m), promote)
          case None => throw new RecordFormatException(lineNo, s"invalid movement expression: ${rest}")
        }
    }

    f(0, s)
  }

  override def parseKi2String(nel: NonEmptyLines): MoveBuilderKi2 = {
    if (nel.lines.length >= 2) {
      throw new RecordFormatException(nel.lines(1)._2, s"too long move expression: ${nel.lines(1)._1}")
    } else {
      val (mv, n) = nel.lines.head
      parseNotation(n, mv)
    }
  }
}
