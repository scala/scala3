import scala.quoted.*

object MatchTest {
  inline def test[T](inline obj: T): String = ${testImpl('obj)}

  def testImpl[T](objExpr: Expr[T])(using qctx: Quotes, t: Type[T]): Expr[String] = {
    import qctx.reflect.*

    val obj = objExpr.asTerm
    val cases = obj.tpe.typeSymbol.children.map { child =>
      val subtype = TypeIdent(child)
      val bind = Symbol.newBind(Symbol.spliceOwner, "c", Flags.EmptyFlags, subtype.tpe)
      CaseDef(Bind(bind, Typed(Ref(bind), subtype)), None, Literal(StringConstant(subtype.show)))
    } ::: {
      CaseDef(Wildcard(), None, Literal(StringConstant("default")))
    } :: Nil
    val bind = Symbol.newBind(Symbol.spliceOwner, "o", Flags.EmptyFlags, obj.tpe)
    val result = Match(obj, cases)
    val code = result.show(using Printer.TreeAnsiCode)
    // println(code)
    result.asExprOf[String]
  }
}