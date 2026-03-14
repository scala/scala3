import scala.quoted.*

object Macro {

    inline def changeIndexWhere[A](inline expr:A):A = ${
        changeIndexWhereImpl('expr)
    }

    def changeIndexWhereImpl[A:Type](expr:Expr[A])(using Quotes):Expr[A] = {
        import quotes.reflect.*
        val r0 = expr.asTerm
        val checker = new TreeMap() {}
        val r = checker.transformTerm(r0)(Symbol.spliceOwner)
        r.asExprOf[A]
    }

}
