import scala.quoted._
import scala.quoted.autolift.given

object Macro {

  inline def optimize[T](x: => T): Any = ${ Macro.impl('x) }

  def impl[T: Type](x: Expr[T])(given QuoteContext): Expr[Any] = {

    def optimize(x: Expr[Any]): Expr[Any] = x match {
      case '{ type $t; ($ls: List[`$t`]).filter($f).filter($g) } =>
        optimize('{ $ls.filter(x => ${Expr.betaReduce(f)('x)} && ${Expr.betaReduce(g)('x)}) })

      case '{ type $t; type $u; type $v; ($ls: List[`$t`]).map[`$u`]($f).map[`$v`]($g) } =>
        optimize('{ $ls.map(x => ${Expr.betaReduce(g)(Expr.betaReduce(f)('x))}) })

      case '{ type $t; ($ls: List[`$t`]).filter($f).foreach[Unit]($g) } =>
        optimize('{ $ls.foreach(x => if (${Expr.betaReduce(f)('x)}) ${Expr.betaReduce(g)('x)} else ()) })

      case _ => x
    }

    val res = optimize(x)

    '{
      val result = $res
      val originalCode = ${x.show}
      val optimizeCode = ${res.show}
      println("Original: " + originalCode)
      println("Optimized: " + optimizeCode)
      println("Result: " + result)
      println()
    }
  }

}
