import scala.quoted._
class Test {type test = Int }
object Macro:
  inline def inlineCall(): Any = ${impl}
  transparent inline def transparentInlineCall(): Any = ${impl}

  /* Returns:
   * val value: Test#test = 0 : Test#test
   * value: Test#test
   */
  def impl(using Quotes): Expr[Any] =
    import quotes.reflect._
    val typeTree = TypeProjection(TypeTree.of[Test], "test")
    val typeRepr = TypeRepr.of[Test#test]
    val sym = Symbol.newVal(Symbol.spliceOwner, "value", typeRepr, Flags.EmptyFlags, Symbol.noSymbol)
    Block(
      List(ValDef(sym, Some(Typed(Literal(IntConstant(0)), typeTree)))),
      Typed(Ref(sym), typeTree)
    ).asExpr
