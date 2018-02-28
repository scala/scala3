import scala.quoted._
import dotty.tools.dotc.quoted.Toolbox._

class Foo {
  def foo: Unit = {
    val e: Expr[Int] = '(3)
    val q = '{ ~( '{ ~e } ) }
    println(q.show)
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    val f = new Foo
    f.foo
  }
}
