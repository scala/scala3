class Opt[+T](v: T)

object Opt:
  def some[T](b: T): Opt[T] = Opt(b)
  val none: Opt[Nothing] = ???

val _: Opt[Int] = .some(10)
val _: Opt[Int] = .none
