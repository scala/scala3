import scala.quoted._

def makeMatch[A: Type](head : Expr[A]) given (qctx : QuoteContext) : Expr[Unit] = {
  import qctx.tasty._

  val sacrifice = '{ $head match { case _ => ??? } }
  sacrifice.unseal

  '{ println("Ok") }
}

def mm(implicit qctx : QuoteContext) = makeMatch('{42})

inline def f = ${ mm }
