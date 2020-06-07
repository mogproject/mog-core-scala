package com.mogproject.mogami.core.game

import com.mogproject.mogami.GamePosition
import com.mogproject.mogami.core.Square
import com.mogproject.mogami.core.move.{Move, Resign, TimeUp}
import com.mogproject.mogami.core.state.{State, StateCache, StateGen}
import org.scalacheck.Gen

import scala.collection.immutable.LazyList

/**
  * Game generator for scalacheck
  */
object GameGen {

  def games(implicit stateCache: StateCache): Gen[Game] = genGame(isFreeMode = false)

  def freeGames(implicit stateCache: StateCache): Gen[Game] = genGame(isFreeMode = true)

  private[this] def genGame(isFreeMode: Boolean)(implicit stateCache: StateCache): Gen[Game] = for {
    gameInfo <- GameInfoGen.infos
    state <- StateGen.statesWithFullPieces
    n <- Gen.choose(0, 50)
    finalAction <- Gen.oneOf(None, None, None, Some(Resign()), Some(TimeUp(Some(1))))
  } yield {
    val moves = movesStream(state, isFreeMode = isFreeMode).take(n)
    val trunk = Branch(state, isFreeMode = isFreeMode).copy(moves = moves.toVector)
    val t = if (trunk.lastState.isMated) trunk else trunk.copy(finalAction = finalAction)
    Game(t, gameInfo = gameInfo)
  }

  private[this] def movesStream(initState: State, isFreeMode: Boolean): LazyList[Move] = {
    type Item = (Option[Move], Option[State], Option[Square])
    val initItem: Item = (None, Some(initState), None)
    lazy val xs: LazyList[Item] = initItem #:: xs.flatMap {
      case (_, Some(s), lastMoveTo) =>
        (for {
          m <- randomMove(s, lastMoveTo)
          ss <- s.makeMove(m, !isFreeMode)
        } yield {
          LazyList((Some(m.copy(elapsedTime = randomTime)), Some(ss), Some(m.to)))
        }).getOrElse(LazyList((None, None, None)))
      case (_, None, _) => LazyList.empty
    }
    xs.tail.takeWhile(_._2.isDefined).map(_._1.get)
  }

  private[this] def randomTime: Option[Int] = Gen.option(Gen.choose(1, 10000)).sample.get

  private[this] def randomMove(s: State, lastMoveTo: Option[Square]): Option[Move] = {
    val moves = s.legalMoves(lastMoveTo)
    if (moves.isEmpty) None else Gen.oneOf(moves).sample
  }

  private[this] def gamePositions(numBranches: Int, maxMoves: Int): Gen[GamePosition] = for {
    br <- Gen.choose(0, 1 + numBranches)
    mv <- Gen.choose(0, maxMoves)
  } yield GamePosition(br, mv)

  private[this] def comments(numBranches: Int, maxMoves: Int): Gen[(GamePosition, String)] = for {
    p <- gamePositions(numBranches, maxMoves)
    s <- Gen.alphaNumStr
  } yield p -> s

  def gamesWithBranch(implicit stateCache: StateCache): Gen[Game] = for {
    gameInfo <- GameInfoGen.infos
    state <- StateGen.statesWithFullPieces
    numTrunkMoves <- Gen.choose(0, 50)
    numBranches <- Gen.choose(0, 5)
    branchPositions <- Gen.listOfN(numBranches, Gen.choose(0, numTrunkMoves))
    branchNumMoves <- Gen.listOfN(numBranches, Gen.choose(0, 20))
    finalAction <- Gen.oneOf(None, None, None, Some(Resign()), Some(TimeUp(Some(1))))
  } yield {
    val moves = movesStream(state, isFreeMode = false).take(numTrunkMoves)
    val trunk = Branch(state).copy(moves = moves.toVector)
    val t = if (trunk.lastState.isMated) trunk else trunk.copy(finalAction = finalAction)
    val branches = branchNumMoves.zip(branchPositions).map { case (m, p) =>
      val pos = trunk.offset + math.min(trunk.moves.length, p)
      val trunkMoves = movesStream(trunk.getState(pos).getOrElse(trunk.initialState), isFreeMode = false).take(m)
      trunk.deriveNewBranch(pos).get.copy(moves = trunkMoves.toVector)
    }.filter(_.moves.nonEmpty).toVector
    Game(t, branches, gameInfo = gameInfo)
  }

  def gamesWithBranchAndComment(implicit stateCache: StateCache): Gen[Game] = for {
    g <- gamesWithBranch
    numComments <- Gen.choose(0, 30)
    comments <- Gen.listOfN(numComments, comments(g.branches.length, 50))
  } yield {
    comments.foldLeft(g) { case (gg, (p, s)) => gg.updateComment(p, s).getOrElse(gg) }
  }

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
    formatVersion.foreach(m += Symbol("formatVersion") -> _)
    blackName.foreach(m += Symbol("blackName") -> _)
    whiteName.foreach(m += Symbol("whiteName") -> _)
    event.foreach(m += Symbol("event") -> _)
    site.foreach(m += Symbol("site") -> _)
    startTime.foreach(m += Symbol("startTime") -> _)
    endTime.foreach(m += Symbol("endTime") -> _)
    timeLimit.foreach(m += Symbol("timeLimit") -> _)
    opening.foreach(m += Symbol("opening") -> _)
    GameInfo(m.result())
  }

}