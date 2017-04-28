package com.mogproject.mogami.core.game

import com.mogproject.mogami._
import com.mogproject.mogami.core.game.Game.HistoryHash
import com.mogproject.mogami.core.state.StateCache.Implicits._
import com.mogproject.mogami.core.state.StateConstant._
import com.mogproject.mogami.core.state.{State, StateCache}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, MustMatchers}

class BranchSpec extends FlatSpec with MustMatchers with GeneratorDrivenPropertyChecks {

  val stateHirateInv = State(WHITE, HIRATE.board, HIRATE.hand)
  val stateEmpty = State(BLACK, Map(), State.EMPTY_HANDS)
  val stateEmptyInv = State(WHITE, Map(), State.EMPTY_HANDS)


  def createBranch(initialState: State,
                   offset: Int = 0,
                   moves: Vector[Move] = Vector.empty,
                   finalAction: Option[SpecialMove] = None,
                   initialHistoryHash: Option[HistoryHash] = None
                  )(implicit stateCache: StateCache): Branch =
    Branch(stateCache.set(initialState), offset, moves, finalAction, initialHistoryHash)


  "Braanch#historyHash" must "create unique hash values for sequences" in {
    createBranch(HIRATE).historyHash mustBe Vector(HIRATE.hash)
    createBranch(HIRATE, 1).historyHash mustBe Vector(HIRATE.hash << 1)
    createBranch(HIRATE, 62).historyHash mustBe Vector(HIRATE.hash << 62 ^ HIRATE.hash >>> 2)
    createBranch(HIRATE, 63).historyHash mustBe Vector(HIRATE.hash)

    val br1 = Branch.parseSfenString("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 0 2g2f 3c3d 2f2e 8c8d 2e2d")
    val br2 = Branch.parseSfenString("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 0 2g2f 8c8d 2f2e 3c3d 2e2d")

    br1.lastState mustBe br2.lastState
    br1.historyHash.last mustNot be(br2.historyHash.last)
    (br1.historyHash.toSet ++ br2.historyHash.toSet).size mustBe 10

    val br3 = createBranch(br1.getState(2).get, 2, initialHistoryHash = Some(br1.historyHash(2)))
      .makeMove(MoveBuilderSfen.parseSfenString("2f2e")).get
      .makeMove(MoveBuilderSfen.parseSfenString("8c8d")).get
      .makeMove(MoveBuilderSfen.parseSfenString("2e2d")).get

    br3.historyHash mustBe br1.historyHash.drop(2)
  }
}
