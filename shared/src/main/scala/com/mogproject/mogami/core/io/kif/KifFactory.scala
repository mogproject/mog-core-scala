package com.mogproject.mogami.core.io.kif

import com.mogproject.mogami.core.io.{IOFactoryLike, Lines, NonEmptyLines}
import com.mogproject.mogami.core.state.StateCache

object KifFactory {
  /**
    * @note do not ignore comments (* or #) here
    */
  def normalizeString(s: Seq[String]): Lines = for {
    (ln, n) <- s.zipWithIndex // set line numbers
    if ln.nonEmpty
  } yield {
    (ln, n + 1)
  }
}

trait KifFactory[T <: KifLike] extends IOFactoryLike {

  final def parseKifString(s: String): T = parseKifString(toLines(s, KifFactory.normalizeString))

  final def parseKifString(lines: Lines): T = parseKifString(toNonEmptyLines(lines))

  def parseKifString(lines: NonEmptyLines): T

}

trait KifGameFactory[T <: KifLike] extends IOFactoryLike {

  final def parseKifString(s: String)(implicit stateCache: StateCache): T = parseKifString(toLines(s, KifFactory.normalizeString))

  final def parseKifString(lines: Lines)(implicit stateCache: StateCache): T = parseKifString(toNonEmptyLines(lines))

  def parseKifString(lines: NonEmptyLines)(implicit stateCache: StateCache): T

}
