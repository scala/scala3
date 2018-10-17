import scala.quoted._

class Foo {
  def foo: Unit = {
    def a: Staged[Int] = '{3}
    def q: Staged[Int] = '{
      val b = 3
      ${
        println("evaluating inner quote")
        '{
          b + $a
        }
      }
    }
    val tb = Toolbox.make
    println(tb.show(q))
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    val f = new Foo
    f.foo
  }
}
