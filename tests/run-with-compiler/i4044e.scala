import scala.quoted._

class Foo {
  def foo: Unit = {
    implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)
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
