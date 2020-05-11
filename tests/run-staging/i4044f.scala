import scala.quoted._
import scala.quoted.staging._

class Foo {
  given Toolbox = Toolbox.make(getClass.getClassLoader)
  def foo: Unit = usingNewScope {
    val e: scope.Expr[Int] = '{3}
    val f: scope.Expr[Int] = '{5}
    def foo(using s: Scope)(x: s.Expr[Int], y: s.Expr[Int]): s.Expr[Int] = '{ $x + $y }
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
