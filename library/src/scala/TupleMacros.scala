package scala

import scala.quoted.*

object TupleMacros {

  /** Macro implementation for type-preserving tuple construction. */
  def applyImpl(args: Expr[Seq[Any]])(using Quotes): Expr[Tuple] =
    args match
      case Varargs(elements) => Expr.ofTupleFromSeq(elements)
      case _ => quotes.reflect.report.errorAndAbort("Expected literal varargs")
}
