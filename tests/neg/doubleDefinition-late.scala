
import annotation.alpha

class Gamma {

  val v = 1
  @alpha("v") val w = 2   // error: double definition

}
// Error when overloading polymorphic and non-polymorphic methods
class Test19 {
  def foo[T <: Int](x: T): T = x
  def foo(x: Int): Int = x // error
}

// Error when overloading polymorphic methods
class Test20 {
  def foo[T <: Int](x: T): T = x
  def foo[S <: Int, T <: Int](x: S): T = ??? // error
}
