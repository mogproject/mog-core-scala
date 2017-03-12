package com.mogproject.mogami.core

import com.mogproject.mogami.core.move.Move
import org.scalacheck.Gen

/**
  * Game generator for scalacheck
  */
object GameGen {

  def games: Gen[Game] = for {
    gameInfo <- GameInfoGen.infos
    state <- StateGen.statesWithFullPieces
    n <- Gen.choose(0, 50)
  } yield {
    val moves = movesStream(state).take(n)
    Game(state, moves.toVector, gameInfo)
  }

  private[this] def movesStream(initState: State): Stream[Move] = {
    type Item = (Option[Move], Option[State])
    val initItem: Item = (None, Some(initState))
    lazy val xs: Stream[Item] = initItem #:: xs.flatMap {
      case (_, Some(s)) =>
        (for {
          m <- randomMove(s)
          ss <- s.makeMove(m)
        } yield {
          Stream((Some(m.copy(elapsedTime = randomTime)), Some(ss)))
        }).getOrElse(Stream((None, None)))
      case (_, None) => Stream.empty
    }
    xs.tail.takeWhile(_._2.isDefined).map(_._1.get)
  }

  private[this] def randomTime: Option[Int] = Gen.option(Gen.choose(1, 10000)).sample.get

  private[this] def randomMove(s: State): Option[Move] = if (s.legalMoves.isEmpty) None else Gen.oneOf(s.legalMoves).sample

}


object GameInfoGen {

  // GameInfo generator for scalacheck
  val infos: Gen[GameInfo] = for {
    formatVersion <- Gen.option(Gen.oneOf("1.0", "2.1", "2.2"))
    blackName <- Gen.option(Gen.alphaStr)
    whiteName <- Gen.option(Gen.alphaStr)
    event <- Gen.option(Gen.alphaStr)
    site <- Gen.option(Gen.alphaStr)
    startTime <- Gen.option(Gen.oneOf("2003/05/03 10:30:00", "2003/05/03"))
    endTime <- Gen.option(Gen.oneOf("2003/05/03 11:11:05", "2003/05/03"))
    timeLimit <- Gen.option(Gen.oneOf("0:25+00", "0:10+01", "0:50+00"))
    opening <- Gen.option(Gen.oneOf("YAGURA", "AIGAKARI"))
  } yield {
    val m = Map.newBuilder[Symbol, String]
    formatVersion.foreach(m += 'formatVersion -> _)
    blackName.foreach(m += 'blackName -> _)
    whiteName.foreach(m += 'whiteName -> _)
    event.foreach(m += 'event -> _)
    site.foreach(m += 'site -> _)
    startTime.foreach(m += 'startTime -> _)
    endTime.foreach(m += 'endTime -> _)
    timeLimit.foreach(m += 'timeLimit -> _)
    opening.foreach(m += 'opening -> _)
    GameInfo(m.result())
  }

}