package repro

import scala.quoted.*

object Crash:
  inline def trigger: Any = ${ triggerImpl }

  private def triggerImpl(using q: Quotes): Expr[Any] =
    import q.reflect.*
    // Build `{ val x: String = 1; x }` with all-NoSpan positions.
    // NoSpan propagates into importSuggestions.deepTest's argument span,
    // triggering the crash in adaptToArgs fallBack via union.startLine.
    val sym = Symbol.newVal(Symbol.spliceOwner, "x", TypeRepr.of[String], Flags.EmptyFlags, Symbol.noSymbol)
    Block(List(ValDef(sym, Some(Literal(IntConstant(1))))), Ref(sym)).asExpr
