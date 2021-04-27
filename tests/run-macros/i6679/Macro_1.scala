import scala.quoted.*

def makeMatch[A: Type](head : Expr[A])(using qctx : Quotes) : Expr[Unit] = {
  import quotes.reflect.*

  val sacrifice = '{ $head match { case _ => ??? } }
  sacrifice.asTerm

  '{ println("Ok") }
}

def mm(implicit qctx : Quotes) = makeMatch('{42})

inline def f = ${ mm }
