import scala.quoted._


inline def f: Any = ${ fImpl }

private def fImpl(using s: Scope): s.Expr[Unit] = {
  import s.tasty._
  searchImplicit(('[A]).tpe) match {
    case x: ImplicitSearchSuccess =>
      '{}
    case x: DivergingImplicit => '{}
      error("DivergingImplicit\n" + x.explanation, rootPosition)
      '{}
    case x: NoMatchingImplicits =>
      error("NoMatchingImplicits\n" + x.explanation, rootPosition)
      '{}
    case x: AmbiguousImplicits =>
      error("AmbiguousImplicits\n" + x.explanation, rootPosition)
      '{}
  }
}

class A
