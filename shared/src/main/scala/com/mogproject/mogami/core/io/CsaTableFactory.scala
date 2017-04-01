package com.mogproject.mogami.core.io

/**
  *
  */
trait CsaTableFactory[T <: CsaLike] extends CsaFactory[T] {

  val csaTable: Seq[String]

  private[this] lazy val tableFactory = BaseTableFactory[T](csaTable)

  override def parseCsaString(nel: NonEmptyLines): T = {
    if (nel.lines.length >= 2) {
      val (ln, n) = nel.lines(1)
      throw new RecordFormatException(n, s"too long expression: ${ln}")
    } else {
      val (ln, n) = nel.lines.head
      tableFactory.get(ln)(apply).getOrElse(throw new RecordFormatException(n, s"invalid input: ${ln}"))
    }
  }

  def apply(id: Int): T

}
