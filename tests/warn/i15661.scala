case class Composite[T](l: List[T], v: T)

def m(composite: Composite[?]): Unit =
  composite match {
    case Composite(l: List[Int], v: Int) => println(v) // warn: type test for List[Int] cannot be checked at runtime
    case _ => println("This is not Int") // was: warn: Unreachable case except for null
  }
