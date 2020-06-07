package com.mogproject.mogami.util

object BitOperation {

  /** Count number of 1 bits. */
  def pop(x: Long): Int = {
    var y = x
    y = y - ((y >> 1) & 0x5555555555555555L)
    y = (y & 0x3333333333333333L) + ((y >> 2) & 0x3333333333333333L)
    y = (y + (y >> 4)) & 0x0f0f0f0f0f0f0f0fL
    y += (y >> 8)
    y += (y >> 16)
    y += (y >> 32)
    (y & 0x000000000000007fL).toInt
  }

  /**
    * Count number of trailing zeros
    *
    * @param x Long number
    * @return ntz
    */
  def ntz(x: Long): Int =
    if (x == 0L) 64 else if (x == 0x8000000000000000L) 63 else (math.log((x & -x).toDouble) / math.log(2)).toInt


  /** Rotate shift left */
  def rotateShift(x: Long, s: Int): Long = x << s | x >>> (64 - s)

}