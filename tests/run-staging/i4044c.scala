import scala.quoted._
import scala.quoted.staging._

class Foo {
  delegate for Toolbox = Toolbox.make(getClass.getClassLoader)
  def foo: Unit = withQuoteContext {
    val q = '{ ${ '{ ${ '{ 5 } } } } }
    println(q.show)
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    val f = new Foo
    f.foo
  }
}
