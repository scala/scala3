import scala.compiletime.ops.int.*
import scala.compiletime.ops.int.S

class Foo[T <: Int] {
  def incP = new Foo[T + 1]
  def incS = new Foo[S[T]]
}
object Foo {
  def apply[T <: Int & Singleton](value : T) : Foo[T] = new Foo[T]
}

val fincS   : Foo[2] = Foo(1).incS
val fincP1  : Foo[2] = Foo(1).incP
val fincP2a = Foo(1).incP
val fincP2b : Foo[2] = fincP2a
val fincP3  : Foo[2] = (new Foo[1]).incP
