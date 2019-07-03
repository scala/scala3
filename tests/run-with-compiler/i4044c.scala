import scala.quoted._

class Foo {
  implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)
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
