package com.mogproject.mogami.core

import org.scalatest.{FlatSpec, MustMatchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

import com.mogproject.mogami._

class PlayerSpec extends FlatSpec with MustMatchers with GeneratorDrivenPropertyChecks {
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

  "Player#parseCsaString" must "make player" in {
    Player.parseCsaString("+") must be(Some(BLACK))
    Player.parseCsaString("-") must be(Some(WHITE))
    Player.parseCsaString("") must be(None)
    Player.parseCsaString(" ") must be(None)
    Player.parseCsaString("x" * 1000) must be(None)
  }

  "Player#parseSfenString" must "make player" in {
    Player.parseSfenString("b") must be(Some(BLACK))
    Player.parseSfenString("w") must be(Some(WHITE))
    Player.parseSfenString("") must be(None)
    Player.parseSfenString(" ") must be(None)
    Player.parseSfenString("x" * 1000) must be(None)
  }

  "Player#unary_!" must "change the player" in {
    !BLACK must be(WHITE)
    !WHITE must be(BLACK)
  }

  it must "cancel double negation" in forAll(PlayerGen.players) { pl =>
    !(!pl) must be(pl)
  }
}