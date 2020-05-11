import scala.quoted._
import scala.quoted.staging._

enum Exp {
  case Num(n: Int)
  case Plus(e1: Exp, e2: Exp)
  case Var(x: String)
  case Let(x: String, e: Exp, in: Exp)
}

object Test {
  import Exp._

  def compile(using s: Scope)(e: Exp, env: Map[String, s.Expr[Int]], keepLets: Boolean): s.Expr[Int] = {
    def compileImpl(using s: Scope)(e: Exp, env: Map[String, s.Expr[Int]]): s.Expr[Int] = e match {
      case Num(n) => Expr(n)
      case Plus(e1, e2) => '{${compileImpl(e1, env)} + ${compileImpl(e2, env)}}
      case Var(x) => env(x)
      case Let(x, e, body) =>
        if (keepLets)
          '{ val y = ${compileImpl(e, env)}; ${compileImpl(body, env + (x -> 'y)) } }
        else
          compileImpl(body, env + (x -> compileImpl(e, env)))
    }
    compileImpl(e, env)
  }


  def main(args: Array[String]): Unit = {
    given Toolbox = Toolbox.make(getClass.getClassLoader)
    val exp = Plus(Plus(Num(2), Var("x")), Num(4))
    val letExp = Let("x", Num(3), exp)

    def res1(using Scope) = '{ (x: Int) => ${compile(exp, Map("x" -> 'x), false)} }


    println(usingNewScope(res1.show))

    val fn = run(res1)
    println(fn(0))
    println(fn(2))
    println(fn(3))

    println("---")

    def res2(using s: Scope) = compile(letExp, Map(), false)
    println(usingNewScope(res2.show))
    println(run(res2))

    println("---")

    def res3(using s: Scope) = compile(letExp, Map(), true)
    println(usingNewScope(res3.show))
    println(run(res3))
  }
}
