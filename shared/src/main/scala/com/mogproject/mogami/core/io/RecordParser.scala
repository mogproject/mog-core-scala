package com.mogproject.mogami.core.io

import com.mogproject.mogami.core.state.State
import com.mogproject.mogami.core.game.{Game, GameInfo}

/**
  *
  */
class RecordParser(sectionSplitter: NonEmptyLines => (Lines, NonEmptyLines, Lines, Option[Line]),
                   gameInfoParser: Lines => GameInfo,
                   initialStateParser: NonEmptyLines => State,
                   moveParser: (State, Lines, Option[Line]) => Game
                  ) {
  def parse(nel: NonEmptyLines): Game = {
    val (gi, st, mv, sp) = sectionSplitter(nel)
    val gameInfo = gameInfoParser(gi)
    val initialState = initialStateParser(st)
    val g = moveParser(initialState, mv, sp)
    g.copy(newGameInfo = gameInfo)
  }
}
