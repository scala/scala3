import scala.quoted.*
import scala.quoted.staging.*

class Foo {
  def foo: Unit = {
    given Compiler = Compiler.make(getClass.getClassLoader)
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
