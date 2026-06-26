sealed trait T
case class C[A](x: A) extends T

def f(t: T) = t match { // warn
  case C(_: Int) => ???
}
