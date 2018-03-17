import scala.quoted._
import dotty.tools.dotc.quoted.Toolbox._

class Foo {
  def foo: Unit = {
    val q = '{ ~( '{ ~( '{ 5 } ) } ) }
    println(q.show)
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    val f = new Foo
    f.foo
  }
}
