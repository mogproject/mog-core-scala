package com.mogproject.mogami.util

/**
  * Implicit conversions
  */
object Implicits {

  /**
    * Extension for built-in Boolean type
    */
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


  /**
    * Extension for built-in Option type
    */
  implicit class RichOption[T](val o: Option[T]) extends AnyVal {
    /**
      * Executes the given binary function if this option value is `Some`.
      */
    final def when[A](f: T => A => A): A => A = o.map(f).getOrElse(identity)
  }


  /**
    * Extension for build-tin Map type
    */
  implicit class RichMap[K, V](val m: Map[K, V]) extends AnyVal {
    /**
      * @return new Map instance whose keys are converted by a function
      */
    final def mapKeys[A](f: K => A): Map[A, V] = m.map { case (k, v) => f(k) -> v }
  }
}
