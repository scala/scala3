import scala.quoted.*


transparent inline def f: Any = ${ fImpl } : Any

private def fImpl(using Quotes) : Expr[Unit] = {
  import quotes.reflect.*
  Implicits.search(TypeRepr.of[A]) match {
    case x: ImplicitSearchSuccess =>
      '{}
    case x: DivergingImplicit => '{}
      report.error("DivergingImplicit\n" + x.explanation, Position.ofMacroExpansion)
      '{}
    case x: NoMatchingImplicits =>
      report.error("NoMatchingImplicits\n" + x.explanation, Position.ofMacroExpansion)
      '{}
    case x: AmbiguousImplicits =>
      report.error("AmbiguousImplicits\n" + x.explanation, Position.ofMacroExpansion)
      '{}
  }
}

class A
