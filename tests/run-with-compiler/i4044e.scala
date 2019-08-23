import scala.quoted._
import scala.quoted.staging._

class Foo {
  delegate for Toolbox = Toolbox.make(getClass.getClassLoader)
  def foo: Unit = withQuoteContext {
    val e: Expr[Int] = '{3}
    val f: Expr[Int] = '{5}
    val t: Type[Int] = '[Int]
    val q = '{ ${ '{ ($e + $f).asInstanceOf[$t] } } }
    println(q.show)
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    val f = new Foo
    f.foo
  }
}
