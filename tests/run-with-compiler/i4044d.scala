import scala.quoted._

class Foo {
  def foo: Unit = {
    implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)
    val a: Expr[Int] = '{3}
    val q: Expr[Int] = '{
      val b = 3
      ${
        println("evaluating inner quote")
        '{
          b + $a
        }
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
