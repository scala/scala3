import scala.quoted.*

object MatchTest {
  inline def test[T](inline obj: T): Unit = ${testImpl('obj)}

  def testImpl[T](objExpr: Expr[T])(using qctx: Quotes, t: Type[T]): Expr[Unit] = {
    import qctx.reflect.*

    val obj = objExpr.asTerm

    val cases = obj.tpe.typeSymbol.children.map { child =>
      val subtype = TypeIdent(child)
      val bind = Symbol.newBind(Symbol.spliceOwner, "c", Flags.EmptyFlags, subtype.tpe)
      CaseDef(Bind(bind, Typed(Ref(bind), subtype)), None, '{()}.asTerm)
    }
    val result = Match(obj, cases)
    //println(result.show(using Printer.TreeAnsiCode))
    result.asExprOf[Unit]
  }
}
