import scala.quoted._
import scala.quoted.staging._

object Test {
  given Toolbox = Toolbox.make(getClass.getClassLoader)
  def eval1(using s: Scope)(ff: s.Expr[Int => Int]): s.Expr[Int] = '{$ff(42)}

  def peval1(using s: Scope)(): s.Expr[Unit] = '{
    def f(x: Int): Int = ${eval1('f)}
  }

  def main(args: Array[String]): Unit = usingNewScope {
    val p = peval1()
    println(p.show)
  }

}