trait L[+T] { def head: T }
class K(val s: String) extends AnyVal
object A {
  def foo: L[String] = ???
  def bar: L[K] = ???
  def baz(k: K): L[String] = ???
}
