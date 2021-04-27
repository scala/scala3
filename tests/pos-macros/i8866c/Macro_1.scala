import scala.quoted.*

def f(xs: Boolean*): Unit = ???

def mcrImpl(using Quotes): Expr[Unit] =
  val func: Expr[Seq[Boolean] => Unit] =
    '{(esx: Seq[Boolean]) => f(esx*)}
  val trees: Expr[Seq[Boolean]] = '{Seq(true)}
  Expr.betaReduce('{ $func($trees) })
end mcrImpl

inline def mcr: Unit = ${ mcrImpl }
