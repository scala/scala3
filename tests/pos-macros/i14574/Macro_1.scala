import scala.quoted.*
object MinimalExample:
  inline def test[T](t: T): Unit = ${ testImpl('t) }

  private def testImpl[T](expr: Expr[T])(using Quotes): Expr[Any] = {

    import quotes.reflect.*

    val companionRef = Ref(expr.asTerm.tpe.classSymbol.get.companionModule)
    Select.unique(companionRef, "unapply").appliedTo(expr.asTerm).asExpr
  }
