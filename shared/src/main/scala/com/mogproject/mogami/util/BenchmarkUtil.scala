package com.mogproject.mogami.util

object BenchmarkUtil {
  def withTime[A](label: String)(thunk: => A): A = {
    val start = System.currentTimeMillis()
    val ret = thunk
    val end = System.currentTimeMillis()
    println(s"${label}: ${(end - start) / 1000.0}s")
    ret
  }

}
