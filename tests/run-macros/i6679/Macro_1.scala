import scala.quoted._

def makeMatch[A: Type](head : Expr[A])(using qctx : Quotes) : Expr[Unit] = {
  import qctx.reflect._

  val sacrifice = '{ $head match { case _ => ??? } }
  Term.of(sacrifice)

  '{ println("Ok") }
}

def mm(implicit qctx : Quotes) = makeMatch('{42})

inline def f = ${ mm }
