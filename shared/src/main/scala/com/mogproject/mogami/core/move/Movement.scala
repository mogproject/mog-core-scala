package com.mogproject.mogami.core.move

/**
  * Movement
  */
object Movement {

  abstract sealed class Movement(val kifString: String)

  case object Dropped extends Movement("打")

  case object Downward extends Movement("引")

  case object Horizontally extends Movement("寄")

  case object Upward extends Movement("上")

  case object Leftwards extends Movement("右")

  case object Rightwards extends Movement("左")

  case object Vertical extends Movement("直")

  case object LeftDownward extends Movement("右引")

  case object LeftHorizontally extends Movement("右寄")

  case object LeftUpward extends Movement("右上")

  case object RightDownward extends Movement("左引")

  case object RightHorizontally extends Movement("左寄")

  case object RightUpward extends Movement("左上")

  lazy val all = Seq(Dropped,
    Downward,
    Horizontally,
    Upward,
    Leftwards,
    Rightwards,
    Vertical,
    LeftDownward,
    LeftHorizontally,
    LeftUpward,
    RightDownward,
    RightHorizontally,
    RightUpward
  )

  private[this] lazy val findTable: Map[String, Movement] = all.map(m => m.kifString -> m).toMap ++ Map("行" -> Upward, "入" -> Upward)

  def find(s: String): Option[Movement] = findTable.get(s)

}
