object `invariant-gadt` {
  case class Invariant[T](value: T)

  def unsound0[T](t: T): T = Invariant(t) match {
    case Invariant(_: Int) =>
      (0: Any) // error
  }

  def unsound1[T](t: T): T = Invariant(t) match {
    case Invariant(_: Int) =>
      0 // error
  }

  def unsound2[T](t: T): T = Invariant(t) match {
    case Invariant(value) => value match {
      case _: Int =>
        0 // error
    }
  }

  def unsoundTwice[T](t: T): T = Invariant(t) match {
    case Invariant(_: Int) => Invariant(t) match {
      case Invariant(_: Int) =>
        0 // error
    }
  }
}
