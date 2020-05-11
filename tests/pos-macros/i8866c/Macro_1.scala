import scala.quoted._

def f(xs: Boolean*): Unit = ???

def mcrImpl(using Scope): scope.Expr[Unit] =
  val func: scope.Expr[Seq[Boolean] => Unit] =
    '{(esx: Seq[Boolean]) => f(esx: _*)}
  val trees: scope.Expr[Seq[Boolean]] = '{Seq(true)}
  Expr.betaReduce('{ $func($trees) })
end mcrImpl

inline def mcr: Unit = ${ mcrImpl }
