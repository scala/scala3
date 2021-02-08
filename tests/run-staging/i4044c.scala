import scala.quoted.*
import scala.quoted.staging.*

class Foo {
  given Compiler = Compiler.make(getClass.getClassLoader)
  def foo: Unit = withQuotes {
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
