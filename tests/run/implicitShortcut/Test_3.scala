import implicitShortcut.*
object Test extends App {

  val d = new Derived
  val b: Base[Int] = d
  implicit val c: C = new C

  assert(b.foo(1) == 42)
  assert(d.foo(1) == 42)
}