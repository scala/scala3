import scala.quoted._
import scala.quoted.matching._

inline def f: Any = ${ fImpl }

private def fImpl given (qctx: QuoteContext): Expr[Unit] = {
  import qctx.tasty._
  searchImplicit(('[A]).unseal.tpe) match {
    case IsImplicitSearchSuccess(x) =>
      '{}
    case IsDivergingImplicit(x) => '{}
      error("DivergingImplicit\n" + x.explanation, rootPosition)
      '{}
    case IsNoMatchingImplicits(x) =>
      error("NoMatchingImplicits\n" + x.explanation, rootPosition)
      '{}
    case IsAmbiguousImplicits(x) =>
      error("AmbiguousImplicits\n" + x.explanation, rootPosition)
      '{}
  }
}

class A
