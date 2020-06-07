package com.mogproject.mogami.core.io.csa

import com.mogproject.mogami.core.Player.BLACK
import com.mogproject.mogami.core.Ptype.KING
import com.mogproject.mogami.core.SquareConstant._
import com.mogproject.mogami.core.game.{Branch, Game, GameInfo}
import com.mogproject.mogami.core.move._
import com.mogproject.mogami.core.state.{State, StateCache}
import com.mogproject.mogami.core.state.StateConstant.HIRATE
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class CsaGameIOSpec extends AnyFlatSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  object TestCsaGameReader extends CsaGameReader

  def createGame(initialState: State,
                 moves: Vector[Move] = Vector.empty,
                 gameInfo: GameInfo = GameInfo(),
                 finalAction: Option[SpecialMove] = None)
                (implicit stateCache: StateCache): Game =
    Game(Branch(stateCache.set(initialState), 0, moves, finalAction), Vector.empty, gameInfo)

  val hirateState = Seq(
    "P1-KY-KE-GI-KI-OU-KI-GI-KE-KY",
    "P2 * -HI *  *  *  *  * -KA * ",
    "P3-FU-FU-FU-FU-FU-FU-FU-FU-FU",
    "P4 *  *  *  *  *  *  *  *  * ",
    "P5 *  *  *  *  *  *  *  *  * ",
    "P6 *  *  *  *  *  *  *  *  * ",
    "P7+FU+FU+FU+FU+FU+FU+FU+FU+FU",
    "P8 * +KA *  *  *  *  * +HI * ",
    "P9+KY+KE+GI+KI+OU+KI+GI+KE+KY",
    "P+",
    "P-",
    "+"
  )

  "CsaGameWriter#toCsaString" must "describe special moves" in StateCache.withCache { implicit cache =>
    createGame(HIRATE, finalAction = Some(Resign())).toCsaString mustBe (hirateState ++ Seq("%TORYO")).mkString("\n")
    createGame(HIRATE, finalAction = Some(Resign(Some(123)))).toCsaString mustBe (hirateState ++ Seq("%TORYO,T123")).mkString("\n")
    createGame(HIRATE, finalAction = Some(TimeUp())).toCsaString mustBe (hirateState ++ Seq("%TIME_UP")).mkString("\n")
    createGame(HIRATE, finalAction = Some(TimeUp(Some(123)))).toCsaString mustBe (hirateState ++ Seq("%TIME_UP,T123")).mkString("\n")
    createGame(HIRATE, finalAction = Some(IllegalMove(
      Move(BLACK, Some(P59), P51, KING, false, false, None, None, false, None, false)
    ))).toCsaString mustBe (hirateState ++ Seq("+5951OU", "%ILLEGAL_MOVE")).mkString("\n")
    createGame(HIRATE, finalAction = Some(IllegalMove(
      Move(BLACK, Some(P59), P51, KING, false, false, None, None, false, Some(123), false)
    ))).toCsaString mustBe (hirateState ++ Seq("+5951OU,T123", "%ILLEGAL_MOVE")).mkString("\n")
  }

  "CsaGameReader#parseMovesCsa" must "parse normal moves" in StateCache.withCache { implicit cache =>
    TestCsaGameReader.parseMoves(HIRATE, Nil, None, isFreeMode = false)(cache) mustBe Game()
  }
  it must "parse special moves" in StateCache.withCache { implicit cache =>
    TestCsaGameReader.parseMoves(HIRATE, CsaFactory.normalizeString(List("%TORYO")), None, isFreeMode = false)(cache) mustBe createGame(HIRATE, finalAction = Some(Resign()))
    TestCsaGameReader.parseMoves(HIRATE, CsaFactory.normalizeString(List("%TORYO,T123")), None, isFreeMode = false)(cache) mustBe createGame(HIRATE, finalAction = Some(Resign(Some(123))))
    TestCsaGameReader.parseMoves(HIRATE, CsaFactory.normalizeString(List("%TIME_UP")), None, isFreeMode = false)(cache) mustBe createGame(HIRATE, finalAction = Some(TimeUp()))
    TestCsaGameReader.parseMoves(HIRATE, CsaFactory.normalizeString(List("%TIME_UP", "T123")), None, isFreeMode = false)(cache) mustBe createGame(HIRATE, finalAction = Some(TimeUp(Some(123))))
    TestCsaGameReader.parseMoves(HIRATE, CsaFactory.normalizeString(List("+5951OU", "%ILLEGAL_MOVE")), None, isFreeMode = false)(cache) mustBe createGame(HIRATE,
      finalAction = Some(IllegalMove(Move(BLACK, Some(P59), P51, KING, false, false, None, None, false, None, false)))
    )
    TestCsaGameReader.parseMoves(HIRATE, CsaFactory.normalizeString(List("+5951OU,T123", "%ILLEGAL_MOVE")), None, isFreeMode = false)(cache) mustBe createGame(HIRATE,
      finalAction = Some(IllegalMove(Move(BLACK, Some(P59), P51, KING, false, false, None, None, false, Some(123), false)))
    )
  }

}
