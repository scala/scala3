

case class Composite[T](v: T)

def m(composite: Composite[_]): Unit =
  composite match {
    case Composite[Int](v) => println(v)  // warn: cannot be checked at runtime
  }

def m2(composite: Composite[_]): Unit =
  composite match {
    case Composite(v) => println(v)  // ok
  }

@main def Test =
  m(Composite("This is String"))
