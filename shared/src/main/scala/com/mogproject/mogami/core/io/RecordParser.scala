package com.mogproject.mogami.core.io

import com.mogproject.mogami.core.state.{State, StateCache}
import com.mogproject.mogami.core.game.{Game, GameInfo}

/**
  *
  */
class RecordParser(sectionSplitter: NonEmptyLines => (Lines, NonEmptyLines, Lines, Option[Line]),
                   gameInfoParser: Lines => GameInfo,
                   initialStateParser: NonEmptyLines => State,
                   moveParser: (State, Lines, Option[Line], Boolean) => StateCache => Game
                  ) {
  def parse(nel: NonEmptyLines, isFreeMode: Boolean)(implicit stateCache: StateCache): Game = {
    val (gi, st, mv, sp) = sectionSplitter(nel)
    val gameInfo = gameInfoParser(gi)
    val initialState = initialStateParser(st)
    val g = moveParser(initialState, mv, sp, isFreeMode)(stateCache)
    g.copy(newGameInfo = gameInfo)
  }
}
