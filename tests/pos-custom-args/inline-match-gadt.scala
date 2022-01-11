object `inline-match-gadt` {
  class Exactly[T]
  erased def exactType[T]: Exactly[T]

  inline def foo[T](t: T): T =
    inline exactType[T] match {
      case _: Exactly[Int] => 23
      case _ => t
    }
}
