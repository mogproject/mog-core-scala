package com.mogproject.mogami.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import com.mogproject.mogami._
import com.mogproject.mogami.core.io.RecordFormatException

class PlayerSpec extends AnyFlatSpec with Matchers with ScalaCheckDrivenPropertyChecks {
  "Player#equals" must "distinguish between black and white" in {
    BLACK.equals(BLACK) must be(true)
    BLACK.equals(WHITE) must be(false)
    WHITE.equals(BLACK) must be(false)
    WHITE.equals(WHITE) must be(true)
  }

  "Player#toString" must "describe black or white" in {
    BLACK.toString must be("BLACK")
    WHITE.toString must be("WHITE")
  }

  "Player#toCsaString" must "describe + or -" in {
    BLACK.toCsaString must be("+")
    WHITE.toCsaString must be("-")
  }

  "Player#toSfenString" must "describe b or w" in {
    BLACK.toSfenString must be("b")
    WHITE.toSfenString must be("w")
  }

  "Player#toSymbolString" must "describe characters" in {
    BLACK.toSymbolString() must be("☗")
    WHITE.toSymbolString() must be("☖")
    BLACK.toSymbolString(false) must be("▲")
    WHITE.toSymbolString(false) must be("△")
  }

  "Player#parseCsaString" must "make player" in {
    Player.parseCsaString("+") mustBe BLACK
    Player.parseCsaString("-") mustBe WHITE
    assertThrows[RecordFormatException](Player.parseCsaString(""))
    assertThrows[RecordFormatException](Player.parseCsaString(" "))
    assertThrows[RecordFormatException](Player.parseCsaString("x" * 1000))
  }

  "Player#parseSfenString" must "make player" in {
    Player.parseSfenString("b") mustBe BLACK
    Player.parseSfenString("w") mustBe WHITE
    assertThrows[RecordFormatException](Player.parseSfenString(""))
    assertThrows[RecordFormatException](Player.parseSfenString(" "))
    assertThrows[RecordFormatException](Player.parseSfenString("x" * 1000))
  }

  "Player#unary_!" must "change the player" in {
    !BLACK mustBe WHITE
    !WHITE mustBe BLACK
  }

  it must "cancel double negation" in forAll(PlayerGen.players) { pl =>
    !(!pl) mustBe pl
  }
}
