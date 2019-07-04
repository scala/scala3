import scala.quoted._

class Foo {
  implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)
  def foo: Unit = withQuoteContext {
    val e: Expr[Int] = '{3}
    val f: Expr[Int] = '{5}
    def foo(x: Expr[Int], y: Expr[Int]): Expr[Int] = '{ $x + $y }
    val q = '{
      val e1 = $e
      val f1 = $f
      ${
        val u = '{2}
        foo('{e1 + $u}, '{f1})
      }
    }
    println(q.show)
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    val f = new Foo
    f.foo
  }
}
