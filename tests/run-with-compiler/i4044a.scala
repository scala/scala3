import scala.quoted._

class Foo {
  def foo: Staged[Any] = {
    implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)
    val e: Expr[Int] = '{3}
    val q = '{ ${ '{ $e } } }
    q
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    val tb = Toolbox.make
    println(tb.show {
      val f = new Foo
      f.foo
    })
  }
}
