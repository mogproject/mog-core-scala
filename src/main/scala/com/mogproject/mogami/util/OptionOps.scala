package com.mogproject.mogami.util

/**
  * Extension for built-in Option type
  */
object OptionOps {

  object Implicits {

    implicit class RichOption[T](val o: Option[T]) extends AnyVal {
      /**
        * Executes the given binary function if this option value is `Some`.
        */
      final def when[A](f: T => A => A): A => A = o.map(f).getOrElse(identity)
    }

  }

}
