enum Opt[+T]:
  case none extends Opt[Nothing]
  case some[T](value: T) extends Opt[T]

  def map[U](f: T => U): Opt[U] = this match
    case .none => .none
    case Opt.some(v) => .some(f(v)) // TODO: This should be `case .some(v) => .some(f(v))`
  end map

object Proxy:
  def opt(o: Opt[Int]): Opt[Int] = o

val _: Opt[Int] = .some(10)
val _: Opt[Any] = .none

val _ = Proxy.opt(.some(10))
val _ = Proxy.opt(.none)
