sealed trait T
case class C[A](x: A) extends T

def f(t: T) = t match { // LTS doesn' warn here
  case C(_: Int) => ???
}
