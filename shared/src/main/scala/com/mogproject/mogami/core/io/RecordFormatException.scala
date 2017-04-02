package com.mogproject.mogami.core.io

/**
  * Customized exception for record errors
  */
class RecordFormatException(line: Int, message: String) extends RuntimeException(s"[line:${line}] ${message}")