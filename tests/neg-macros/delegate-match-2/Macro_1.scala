import scala.quoted._


inline def f: Any = ${ fImpl }

private def fImpl (using qctx: QuoteContext) : Expr[Unit] = {
  import qctx.tasty._
  Implicits.search(('[A]).unseal.tpe) match {
    case x: ImplicitSearchSuccess =>
      '{}
    case x: DivergingImplicit => '{}
      Reporting.error("DivergingImplicit\n" + x.explanation, rootPosition)
      '{}
    case x: NoMatchingImplicits =>
      Reporting.error("NoMatchingImplicits\n" + x.explanation, rootPosition)
      '{}
    case x: AmbiguousImplicits =>
      Reporting.error("AmbiguousImplicits\n" + x.explanation, rootPosition)
      '{}
  }
}

class A
