import scala.quoted.*

object Macro {

  inline def optimize[T](inline x: T): Any = ${ Macro.impl('x) }

  def impl[T: Type](x: Expr[T])(using Quotes): Expr[Any] = {

    def optimize(x: Expr[Any]): Expr[Any] = x match {
      case '{ type t; ($ls: List[`t`]).filter($f).filter($g) } =>
        optimize('{ $ls.filter(x => ${Expr.betaReduce('{$f(x)})} && ${Expr.betaReduce('{$g(x)})}) })

      case '{ type t; type u; type v; ($ls: List[`t`]).map[`u`]($f).map[`v`]($g) } =>
        optimize('{ $ls.map(x => ${Expr.betaReduce('{$g(${Expr.betaReduce('{$f(x)})})})}) })

      case '{ type t; ($ls: List[`t`]).filter($f).foreach[Unit]($g) } =>
        optimize('{ $ls.foreach(x => if (${Expr.betaReduce('{$f(x)})}) ${Expr.betaReduce('{$g(x)})} else ()) })

      case _ => x
    }

    val res = optimize(x)

    '{
      val result = $res
      val originalCode = ${Expr(x.show)}
      val optimizeCode = ${Expr(res.show)}
      println("Original: " + originalCode)
      println("Optimized: " + optimizeCode)
      println("Result: " + result)
      println()
    }
  }

}
