import quoted._
import scala.quoted.staging._

object Test {
  def main(args: Array[String]): Unit = {
    given Toolbox = Toolbox.make(getClass.getClassLoader)
    def q(using s: Scope) = f
    println(run(q))
    println(usingNewScope(q.show))
  }

  def f(using s: Scope): s.Expr[Int] = '{
    def ff: Int = {
      $g
    }
    ff
  }

  def g(using s: Scope): s.Expr[Int] = '{
    val a = 9
    a + 0
  }
}
