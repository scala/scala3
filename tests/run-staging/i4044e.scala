import scala.quoted._
import scala.quoted.staging._

class Foo {
  given Toolbox = Toolbox.make(getClass.getClassLoader)
  def foo: Unit = withQuotes {
    val e: Expr[Int] = '{3}
    val f: Expr[Int] = '{5}
    val t: Type[Int] = Type.of[Int]
    val q = '{ ${ '{ ($e + $f).asInstanceOf[t.Underlying] } } }
    println(q.show)
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    val f = new Foo
    f.foo
  }
}
