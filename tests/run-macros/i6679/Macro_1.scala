import scala.quoted._

def makeMatch[A](using s: Scope)(head: s.Expr[A])(using s.Type[A]): s.Expr[Unit] = {
  import s.tasty._

  val sacrifice = '{ $head match { case _ => ??? } }
  sacrifice

  '{ println("Ok") }
}

def mm(implicit s: Scope) = makeMatch('{42})

inline def f = ${ mm }
