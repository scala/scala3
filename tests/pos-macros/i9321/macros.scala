import scala.quoted._

type Foo
type F[X]
def varargsFunc(funcs0: Foo*) = ???

inline def mcr1: F[String] = ${ mcr1Impl }
def mcr1Impl(using s: Scope): s.Expr[F[String]] = '{???}

inline def mcr2: Unit = ${mcr2Impl}
def mcr2Impl(using s: Scope): s.Expr[Unit] =
  val func: s.Expr[Seq[Foo] => Unit] =
    '{ (esx: Seq[Foo]) => varargsFunc(esx: _*) }
  val trees: s.Expr[Seq[Foo]] =
    '{Nil}
  Expr.betaReduce('{$func($trees)})