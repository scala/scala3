import scala.language.experimental.unqualifiedSelectors

enum Opt[+T]:
  case none extends Opt[Nothing]
  case some[T](value: T) extends Opt[T]
  

  def map[U](f: T => U): Opt[U] = this match
    case .none => .none
    case .some(v) => .some(f(v))
  end map
