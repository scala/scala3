enum Opt[+T]:
  case none extends Opt[Nothing]
  case some[T](value: T) extends Opt[T]

object Proxy:
  def opt(o: Opt[Int]): Opt[Int] = o

val _: Opt[Int] = .some(10)
val _: Opt[Any] = .none

val _ = Proxy.opt(.some(10))
val _ = Proxy.opt(.none)
