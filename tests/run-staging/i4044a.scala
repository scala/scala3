import scala.quoted.*
import scala.quoted.staging.*

class Foo {
  given Compiler = Compiler.make(getClass.getClassLoader)
  def foo: Unit = withQuotes {
    val e: Expr[Int] = '{3}
    val q = '{ ${ '{ $e } } }
    println(q.show)
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    val f = new Foo
    f.foo
  }
}
