trait T[X] {
  def foo(x: X): X
}
trait U[X] extends T[X] {
  abstract override def foo(x: X): X = super.foo(x)
}
abstract class A extends U[String] // error

