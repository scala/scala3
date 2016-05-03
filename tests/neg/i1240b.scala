// yet another variant, testing type parameters
trait T[X] {
  def foo(x: X): X
}
abstract class A[X] extends T[X] {
  def foo(x: X): X           = {println("A.X"); x}
  def foo(x: String): String = {println("A.String"); x}
}
trait U[X] extends T[X] {
  abstract override def foo(x: X): X = super.foo(x)
}
object Test extends A[String] with U[String] // error: accidental override
