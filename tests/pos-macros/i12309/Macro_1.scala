import scala.quoted.*

object TestMacro {
  def use(f: () => String): Unit = ()

  inline def test: Unit = ${testImpl}

  def testImpl(using Quotes): Expr[Unit] = {
    import quotes.reflect.*

    def resultDefBody(): Term = '{
      val result: String = "xxx"
      result
    }.asTerm
    val resultDefSymbol = Symbol.newMethod(Symbol.spliceOwner, "getResult", MethodType(Nil)(_ => Nil, _ => TypeRepr.of[String]))
    val resultDef = DefDef(resultDefSymbol, { case _ => Some(resultDefBody().changeOwner(resultDefSymbol)) })
    val resultExpr = Block(List(resultDef), Closure(Ref(resultDefSymbol), None)).asExprOf[() => String]

    //

    val r = '{ TestMacro.use($resultExpr) }
    // println(r.asTerm.show(using Printer.TreeShortCode))
    r
  }
}
