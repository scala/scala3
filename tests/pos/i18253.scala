import scala.compiletime.ops.int.Max

trait Foo[A]
trait Bar[B]:
  type Out <: Int
object Bar:
  given [C <: Int] => Bar[C]:
    type Out = C

class Test:
  def mkFoo(using bx: Bar[2]): Foo[Max[1, bx.Out]] = ???
  def check[Y](yy: Y, clue: Int = 1): Unit = ()

  def test: Unit = check(mkFoo)
