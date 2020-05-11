
import scala.quoted._

object Exp {

  private def compileImpl(using s: Scope)(e: s.Expr[Int], env: Map[String, s.Expr[Int]]): s.Expr[Int] = {
    e match {
      case '{$s:Int}  => s
      case exp =>
        compileImpl(exp, env)
    }
  }

  private def compileUnlift(using s: Scope)(e: s.Expr[Int]): s.Expr[Int] = {
    val environment = Map[String, s.Expr[Int]]()
    compileImpl(e, environment)
  }

  inline def compile(inline expr: Int): Int = {
    ${compileUnlift('expr)}
  }

}
