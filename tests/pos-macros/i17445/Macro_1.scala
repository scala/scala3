package i17445

import scala.quoted.*
import scala.collection.*


object Macro {

    inline def changeIndexWhere[A](inline expr: A): A =
      ${ changeIndexWhereImpl('expr) }

    def changeIndexWhereImpl[A: Type](expr: Expr[A])(using Quotes): Expr[A] = {
        import quotes.reflect.*
        val r0 = expr.asTerm
        val checker = new TreeMap() {}
        println(r0.getClass().getSimpleName())
        val r = checker.transformTerm(r0)(Symbol.spliceOwner)
        r.asExprOf[A]
    }

}