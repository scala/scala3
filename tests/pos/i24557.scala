object test {
  sealed trait X[T]
  trait Iterable[T]
  case class I[T, C <: Iterable[T]]() extends X[C]

  def t[T](i: X[T]): Unit =
    i match
      case i @ I() => ()
}
