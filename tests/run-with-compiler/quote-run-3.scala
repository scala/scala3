
import scala.quoted._

object Test {
  def main(args: Array[String]): Unit = {
    val tb = Toolbox.make

    def expr(a: Expr[Int]): Staged[Int] = '{
      val b = 3
      println("foo")
      2 + b + ~{
        println("splice: " + a)
        a
      }
    }
    println("start")
    val res = tb.run {
      println("start in expr")
      val e = expr('(4))
      println("end in expr")
      e
    }
    println("res: " + res)
    println("end")

  }
}
