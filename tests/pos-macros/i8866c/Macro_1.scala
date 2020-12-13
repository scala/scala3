import scala.quoted._

def f(xs: Boolean*): Unit = ???

def mcrImpl(using Quotes): Expr[Unit] =
  val func: Expr[Seq[Boolean] => Unit] =
    '{(esx: Seq[Boolean]) => f(esx: _*)}
  val trees: Expr[Seq[Boolean]] = '{Seq(true)}
  Expr.betaReduce('{ $func($trees) })
end mcrImpl

inline def mcr: Unit = ${ mcrImpl }
