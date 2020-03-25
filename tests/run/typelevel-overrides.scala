trait T {
  def f(x: Int): Int
}
trait U extends T {
  override def f(x: Int): Int = x + 1
}
class A extends T {
  def f(x: Int) = x
}
class B extends A {
  override inline def f(x: Int) = inline x match {
    case 0 => 0
    case x => x + 2
  }
}
class C extends A with U {
  override inline def f(x: Int) = inline x match {
    case 0 => 0
    case x => x + 2
  }
}
object Test extends App {
  val a: A = new B
  assert(a.f(0) == 2, a.f(0))
  assert(a.f(1) == 3)
  val b: B = new B
  assert(b.f(0) == 0)
  assert(b.f(1) == 3)
  val c: A = new C
  assert(c.f(0) == 2, c.f(0))
  assert(c.f(1) == 3, c.f(1))
}