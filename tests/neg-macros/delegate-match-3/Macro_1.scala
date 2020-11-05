import scala.quoted._


inline def f: Any = ${ fImpl }

private def fImpl(using qctx: QuoteContext) : Expr[Unit] = {
  import qctx.reflect._
  Implicits.search((Type[A]).unseal.tpe) match {
    case x: ImplicitSearchSuccess =>
      '{}
    case x: DivergingImplicit => '{}
      Reporting.error("DivergingImplicit\n" + x.explanation, Position.ofMacroExpansion)
      '{}
    case x: NoMatchingImplicits =>
      Reporting.error("NoMatchingImplicits\n" + x.explanation, Position.ofMacroExpansion)
      '{}
    case x: AmbiguousImplicits =>
      Reporting.error("AmbiguousImplicits\n" + x.explanation, Position.ofMacroExpansion)
      '{}
  }
}

class A
