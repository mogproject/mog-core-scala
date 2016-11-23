package com.mogproject.mogami.core

import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import com.mogproject.mogami.core.Player.{BLACK, WHITE}

class PlayerSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  "Player#equals" should "distinguish between black and white" in {
    BLACK.equals(BLACK) should be(true)
    BLACK.equals(WHITE) should be(false)
    WHITE.equals(BLACK) should be(false)
    WHITE.equals(WHITE) should be(true)
  }

  "Player#toString" should "describe black or white" in {
    BLACK.toString should be("BLACK")
    WHITE.toString should be("WHITE")
  }

  "Player#toCsaString" should "describe + or -" in {
    BLACK.toCsaString should be("+")
    WHITE.toCsaString should be("-")
  }

  "Player#toSfenString" should "describe b or w" in {
    BLACK.toSfenString should be("b")
    WHITE.toSfenString should be("w")
  }

  "Player#parseCsaString" should "make player" in {
    Player.parseCsaString("+") should be(Some(BLACK))
    Player.parseCsaString("-") should be(Some(WHITE))
    Player.parseCsaString("") should be(None)
    Player.parseCsaString(" ") should be(None)
    Player.parseCsaString("x" * 1000) should be(None)
  }

  "Player#parseSfenString" should "make player" in {
    Player.parseSfenString("b") should be(Some(BLACK))
    Player.parseSfenString("w") should be(Some(WHITE))
    Player.parseSfenString("") should be(None)
    Player.parseSfenString(" ") should be(None)
    Player.parseSfenString("x" * 1000) should be(None)
  }

  "Player#unary_!" should "change the player" in {
    !BLACK should be(WHITE)
    !WHITE should be(BLACK)
  }

  it should "cancel double negation" in forAll(PlayerGen.players) { pl =>
    !(!pl) should be(pl)
  }
}
