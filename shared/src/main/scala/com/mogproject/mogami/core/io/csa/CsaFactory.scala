package com.mogproject.mogami.core.io.csa

import com.mogproject.mogami.core.io.{IOFactoryLike, Lines, NonEmptyLines}
import com.mogproject.mogami.core.state.StateCache

/**
  * Reads CSA-formatted string
  *
  * An exception can be thrown.
  */
object CsaFactory {
  def normalizeString(s: Seq[String]): Lines = for {
    (ln, n) <- s.zipWithIndex // set line numbers
    if !ln.startsWith("'") // ignore comment lines
    chunk <- ln.split(',')
    if chunk.nonEmpty
  } yield {
    (chunk, n + 1)
  }
}

trait CsaFactory[T <: CsaLike] extends IOFactoryLike {

  final def parseCsaString(s: String): T = parseCsaString(toLines(s, CsaFactory.normalizeString))

  final def parseCsaString(lines: Lines): T = parseCsaString(toNonEmptyLines(lines))

  def parseCsaString(lines: NonEmptyLines): T

}

trait CsaGameFactory[T <: CsaLike] extends IOFactoryLike {

  final def parseCsaString(s: String)(implicit stateCache: StateCache): T = parseCsaString(toLines(s, CsaFactory.normalizeString))

  final def parseCsaString(lines: Lines)(implicit stateCache: StateCache): T = parseCsaString(toNonEmptyLines(lines))

  def parseCsaString(lines: NonEmptyLines)(implicit stateCache: StateCache): T

}
