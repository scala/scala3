//> using options -Xfatal-warnings

case class Composite[T](v: T)

def m(composite: Composite[?]): Unit =
  composite match {
    case Composite[Int](v) => println(v)  // error: cannot be checked at runtime
  }

def m2(composite: Composite[?]): Unit =
  composite match {
    case Composite(v) => println(v)  // ok
  }

@main def Test =
  m(Composite("This is String"))
