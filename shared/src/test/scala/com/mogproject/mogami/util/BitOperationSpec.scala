package com.mogproject.mogami.util

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class BitOperationSpec extends AnyFlatSpec with Matchers {
  "BitOperation#ntz" must "return the number of trailing zeros" in {
    BitOperation.ntz(0x0L) mustBe 64
    BitOperation.ntz(0x1L) mustBe 0
    BitOperation.ntz(0xffffffffffffffffL) mustBe 0
    BitOperation.ntz(0xfffffffffffffff8L) mustBe 3
    BitOperation.ntz(0xc0L) mustBe 6
    BitOperation.ntz(0xfffffff000000000L) mustBe 36
  }
}
