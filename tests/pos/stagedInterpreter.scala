import scala.quoted._

enum Exp {
  case Num(n: Int)
  case Plus(e1: Exp, e2: Exp)
  case Var(x: String)
}

object Test {
  import Exp._

  val exp = Plus(Plus(Num(2), Var("x")), Num(4))

  def compile(e: Exp, env: Map[String, Expr[Int]]): Expr[Int] = e match {
    case Num(n) => n
    case Plus(e1, e2) => '(~compile(e1, env) + ~compile(e2, env))
    case Var(x) => env(x)
  }

  val res = (x: Int) => ~compile(exp, Map("x" -> '(x)))

}
