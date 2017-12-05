import scala.quoted._

enum Exp {
  case Num(n: Int)
  case Plus(e1: Exp, e2: Exp)
  case Var(x: String)
  case Let(x: String, e: Exp, in: Exp)
}

object Test {
  import Exp._

  val keepLets = true

  val exp = Plus(Plus(Num(2), Var("x")), Num(4))

  val letExp = Let("x", Num(3), exp)

  def compile(e: Exp, env: Map[String, Expr[Int]]): Expr[Int] = e match {
    case Num(n) => n
    case Plus(e1, e2) => '(~compile(e1, env) + ~compile(e2, env))
    case Var(x) => env(x)
    case Let(x, e, body) =>
      if (keepLets)
        '{ val y = ~compile(e, env); ~compile(body, env + (x -> '(y))) }
      else
        compile(body, env + (x -> compile(e, env)))
  }

  val res1 = '{ (x: Int) => ~compile(exp, Map("x" -> '(x))) }

  val res2 = compile(letExp, Map())

  res1.run
}
