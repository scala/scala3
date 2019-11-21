import scala.quoted._
import scala.quoted.matching._

inline def f: Any = ${ fImpl }

private def fImpl(given qctx: QuoteContext): Expr[Unit] = {
  import qctx.tasty.{_, given}
  searchImplicit(('[A]).unseal.tpe) match {
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
