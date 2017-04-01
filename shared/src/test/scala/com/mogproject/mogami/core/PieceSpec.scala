package com.mogproject.mogami.core

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, MustMatchers}
import com.mogproject.mogami._
import com.mogproject.mogami.core.io.RecordFormatException

class PieceSpec extends FlatSpec with MustMatchers with GeneratorDrivenPropertyChecks {
  val allPieces: Seq[Piece] = for {p <- Player.constructor; pt <- Ptype.constructor} yield Piece(p, pt)

  val csaPieceTypes = Seq("TO", "NY", "NK", "NG", "UM", "RY", "OU", "KI", "FU", "KY", "KE", "GI", "KA", "HI")
  val csaPieces: Seq[String] = for (t <- Seq("+", "-"); p <- csaPieceTypes) yield t + p
  val sfenPieces: Seq[String] = Seq(
    "+P", "+L", "+N", "+S", "+B", "+R", "K", "G", "P", "L", "N", "S", "B", "R",
    "+p", "+l", "+n", "+s", "+b", "+r", "k", "g", "p", "l", "n", "s", "b", "r"
  )
  val kifPieces: Seq[String] = Seq(
    " と", " 杏", " 圭", " 全", " 馬", " 龍", " 玉", " 金", " 歩", " 香", " 桂", " 銀", " 角", " 飛",
    "vと", "v杏", "v圭", "v全", "v馬", "v龍", "v玉", "v金", "v歩", "v香", "v桂", "v銀", "v角", "v飛"
  )

  "Piece#parseCsaString" must "succeed in normal cases" in {
    csaPieces map { c => Piece.parseCsaString(c) } mustBe allPieces
  }
  it must "return None in error cases" in {
    assertThrows[RecordFormatException](Piece.parseCsaString(""))
    assertThrows[RecordFormatException](Piece.parseCsaString(" "))
    assertThrows[RecordFormatException](Piece.parseCsaString("x" * 1000))
    assertThrows[RecordFormatException](Piece.parseCsaString("=FU"))
    assertThrows[RecordFormatException](Piece.parseCsaString("-Fu"))
    assertThrows[RecordFormatException](Piece.parseCsaString("-FU+"))
  }
  "Piece#toCsaString" must "describe in csa format" in {
    allPieces map (_.toCsaString) must be(csaPieces)
  }
  it must "recover the original piece" in forAll(PieceGen.pieces) { p =>
    Piece.parseCsaString(p.toCsaString) mustBe p
  }

  "Piece#toSfenString" must "describe in SFEN format" in {
    allPieces map (_.toSfenString) must be(sfenPieces)
  }
  "Piece#parseSfenString" must "succeed in normal cases" in {
    sfenPieces map { c => Piece.parseSfenString(c) } must be(allPieces map (Some(_)))
  }
  it must "return None in error cases" in {
    assertThrows[RecordFormatException](Piece.parseSfenString(""))
    assertThrows[RecordFormatException](Piece.parseSfenString(" "))
    assertThrows[RecordFormatException](Piece.parseSfenString("x" * 1000))
    assertThrows[RecordFormatException](Piece.parseSfenString("=FU"))
    assertThrows[RecordFormatException](Piece.parseSfenString("-Fu"))
    assertThrows[RecordFormatException](Piece.parseSfenString("-FU+"))
  }
  it must "recover the original piece" in forAll(PieceGen.pieces) { p =>
    Piece.parseSfenString(p.toSfenString) must be(Some(p))
  }
  "Piece#toKifString" must "describe pieces in KIF format" in {
    allPieces map (_.toKifString) mustBe kifPieces
  }
  "Piece#parseKifString" must "succeed in normal cases" in {
    kifPieces map { c => Piece.parseKifString(c) } mustBe allPieces
  }
  it must "throw an exception in error cases" in {
    assertThrows[RecordFormatException](Piece.parseKifString(""))
    assertThrows[RecordFormatException](Piece.parseKifString(" "))
    assertThrows[RecordFormatException](Piece.parseKifString("x" * 1000))
    assertThrows[RecordFormatException](Piece.parseKifString("=FU"))
    assertThrows[RecordFormatException](Piece.parseKifString("-Fu"))
    assertThrows[RecordFormatException](Piece.parseKifString("-FU+"))
  }
  it must "recover the original piece" in forAll(PieceGen.pieces) { p =>
    Piece.parseKifString(p.toKifString) mustBe p
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

