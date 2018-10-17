import scala.quoted._

class Foo {
  def foo: Unit = {
    val tb = Toolbox.make
    val q = '{ ${ '{ ${ '{ 5 } } } } }
    println(tb.show(q))
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    val f = new Foo
    f.foo
  }
}
