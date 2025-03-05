import quoted.*

object Macro:
  inline def impl(inline expr: Any): Any =
    ${implImpl('expr)}

  def implImpl(expr: Expr[Any])(using q: Quotes): Expr[Any] =
    import q.reflect.*
    expr.asTerm.asInstanceOf[Inlined].body match
      // this should not fail with a MatchError
      case TypeBlock(_, _) => '{ "TypeBlock" }
      case _ => '{ "Nothing" }
