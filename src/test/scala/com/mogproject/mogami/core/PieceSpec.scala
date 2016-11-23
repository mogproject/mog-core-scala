package com.mogproject.mogami.core

import com.mogproject.mogami.core.Player.{BLACK, WHITE}
import com.mogproject.mogami.core.Ptype.{PAWN, PPAWN}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, MustMatchers}

class PieceSpec extends FlatSpec with MustMatchers with GeneratorDrivenPropertyChecks {
  val allPieces: Seq[Piece] = for {p <- Player.constructor; pt <- Ptype.constructor} yield Piece(p, pt)

  val csaPieceTypes = Seq("TO", "NY", "NK", "NG", "UM", "RY", "OU", "KI", "FU", "KY", "KE", "GI", "KA", "HI")
  val csaPieces: Seq[String] = for (t <- Seq("+", "-"); p <- csaPieceTypes) yield t + p
  val sfenPieces: Seq[String] = Seq(
    "+P", "+L", "+N", "+S", "+B", "+R", "K", "G", "P", "L", "N", "S", "B", "R",
    "+p", "+l", "+n", "+s", "+b", "+r", "k", "g", "p", "l", "n", "s", "b", "r"
  )

  "Piece#parseCsaString" must "succeed in normal cases" in {
    csaPieces map { c => Piece.parseCsaString(c) } must be(allPieces map (Some(_)))
  }
  it must "return None in error cases" in {
    Piece.parseCsaString("") must be(None)
    Piece.parseCsaString(" ") must be(None)
    Piece.parseCsaString("x" * 1000) must be(None)
    Piece.parseCsaString("=FU") must be(None)
    Piece.parseCsaString("-Fu") must be(None)
    Piece.parseCsaString("-FU+") must be(None)
  }
  "Piece#toCsaString" must "describe in csa format" in {
    allPieces map (_.toCsaString) must be(csaPieces)
  }
  it must "recover the original piece" in forAll(PieceGen.pieces) { p =>
    Piece.parseCsaString(p.toCsaString) must be(Some(p))
  }

  "Piece#parseSfenString" must "succeed in normal cases" in {
    sfenPieces map { c => Piece.parseSfenString(c) } must be(allPieces map (Some(_)))
  }
  it must "return None in error cases" in {
    Piece.parseSfenString("") must be(None)
    Piece.parseSfenString(" ") must be(None)
    Piece.parseSfenString("x" * 1000) must be(None)
    Piece.parseSfenString("=FU") must be(None)
    Piece.parseSfenString("-Fu") must be(None)
    Piece.parseSfenString("-FU+") must be(None)
  }
  "Piece#toSfenString" must "describe in csa format" in {
    allPieces map (_.toSfenString) must be(sfenPieces)
  }
  it must "recover the original piece" in forAll(PieceGen.pieces) { p =>
    Piece.parseSfenString(p.toSfenString) must be(Some(p))
  }

  "Piece#unary_!" must "flip the owner" in {
    !Piece(BLACK, PAWN) must be(Piece(WHITE, PAWN))
    !Piece(WHITE, PAWN) must be(Piece(BLACK, PAWN))
  }

  "Piece#promoted" must "promote the piece type" in {
    Piece(BLACK, PAWN).promoted must be(Piece(BLACK, PPAWN))
    Piece(BLACK, PPAWN).promoted must be(Piece(BLACK, PPAWN))
  }

  "Piece#demoted" must "demote the piece type" in {
    Piece(BLACK, PAWN).demoted must be(Piece(BLACK, PAWN))
    Piece(BLACK, PPAWN).demoted must be(Piece(BLACK, PAWN))
  }
}

