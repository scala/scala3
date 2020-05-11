import scala.quoted._
import scala.quoted.staging._

class Foo {
  given Toolbox = Toolbox.make(getClass.getClassLoader)
  def foo: Unit = usingNewScope {
    val e: scope.Expr[Int] = '{3}
    val f: scope.Expr[Int] = '{5}
    val t: scope.Type[Int] = '[Int]
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
