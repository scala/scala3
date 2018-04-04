import quoted._
import dotty.tools.dotc.quoted.Toolbox._

object Test {
  def main(args: Array[String]): Unit = {
    val q = f
    println(q.run)
    println(q.show)
  }

  def f: Expr[Int] = '{
    def ff: Int = {
      ~g
    }
    ff
  }

  def g: Expr[Int] = '{
    val a = 9
    a + 0
  }
}
