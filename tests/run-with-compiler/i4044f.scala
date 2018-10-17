import scala.quoted._

class Foo {
  def foo: Unit = {
    def e: Staged[Int] = '{3}
    def f: Staged[Int] = '{5}
    def foo(x: Expr[Int], y: Expr[Int]): Staged[Int] = '{ $x + $y }
    def q: Staged[Int] = '{
      val e1 = $e
      val f1 = $f
      ${
        val u = '{2}
        foo('{e1 + $u}, '{f1})
      }
    }
    val tb = Toolbox.make
    println(tb.show(q))
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    val f = new Foo
    f.foo
  }
}
