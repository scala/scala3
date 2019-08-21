import scala.quoted._
import scala.quoted.staging._

class Foo {
  def foo: Unit = {
    implicit val toolbox: scala.quoted.staging.Toolbox = scala.quoted.staging.Toolbox.make(getClass.getClassLoader)
    run {
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
      '{}
    }
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    val f = new Foo
    f.foo
  }
}
