import quoted._
import dotty.tools.dotc.quoted.Toolbox._

object Test {
  def main(args: Array[String]): Unit = {
    val q = '{
      val x = 1
      println(~{
        println(1)
        val a = '(x)
        val b = '(x)
        '{ ~a + ~b }
      })
    }

    println(q.show)
  }
}
