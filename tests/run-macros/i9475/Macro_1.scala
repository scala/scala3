
import scala.quoted.*

object Exp {

  private def compileImpl(e: Expr[Int], env: Map[String, Expr[Int]])(using Quotes): Expr[Int] = {
    e match {
      case '{$s:Int}  => s
      case exp =>
        compileImpl(exp, env)
    }
  }

  private def compileUnlift(e: Expr[Int])(using Quotes): Expr[Int] = {
    val environment = Map[String, Expr[Int]]()
    compileImpl(e, environment)
  }

  inline def compile(inline expr: Int): Int = {
    ${compileUnlift('expr)}
  }

}
