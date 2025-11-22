trait TC2[X]
sealed trait TC1[T]
case class Iterable[T, C <: scala.Iterable[T]](tag: TC1[C]) extends TC1[C]
