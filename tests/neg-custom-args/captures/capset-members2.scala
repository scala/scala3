import language.experimental.captureChecking
import caps.*

trait Foo:
  type C^[T] // error

  def foo[C^[_]]: Int // error

  def bar [M[_], C^ <: M]: Int // error