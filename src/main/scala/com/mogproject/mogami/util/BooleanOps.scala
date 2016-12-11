package com.mogproject.mogami.util

/**
  * Extension for built-in Boolean type
  */
object BooleanOps {

  object Implicits {

    implicit class RichBoolean(val b: Boolean) extends AnyVal {
      /**
        * @return the given argument in `Some` if this is `true`, `None` otherwise.
        */
      final def option[A](a: => A): Option[A] = if (b) Some(a) else None

      /**
        * @return `t` if true, `f` otherwise
        */
      final def fold[A](t: => A, f: => A): A = if (b) t else f

      /**
        * Executes the given binary function if this boolean value is `true`.
        */
      final def when[A](f: A => A): A => A = if (b) f else identity
    }

  }

}
