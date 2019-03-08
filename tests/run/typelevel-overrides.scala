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
    case x => x
  }
}
class C extends A with U {
  override inline def f(x: Int) = inline x match {
    case 0 => 0
    case x => x
  }
}
object Test extends App {
  val a: A = new B
  assert(a.f(0) == 0)
  val b: B = new B
  assert(b.f(0) == 0)
  val c: A = new C
  assert(c.f(0) == 1, c.f(0))
}