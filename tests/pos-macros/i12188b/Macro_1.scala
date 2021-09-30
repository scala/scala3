import scala.quoted.*

object MatchTest {
  inline def test[T](inline obj: Any): Unit = ${testImpl('obj)}

  def testImpl[T](objExpr: Expr[T])(using Quotes): Expr[Unit] = {
    import quotes.reflect.*
    // test that the extractors work
    val Inlined(None, Nil, Block(Nil, Match(param @ Ident("a"), List(CaseDef(Literal(IntConstant(1)), None, Block(Nil, Literal(UnitConstant()))), CaseDef(Wildcard(), None, Block(Nil, Literal(UnitConstant()))))))) = objExpr.asTerm
    // test that the constructors work
    Block(Nil, Match(param, List(CaseDef(Literal(IntConstant(1)), None, Block(Nil, Literal(UnitConstant()))), CaseDef(Wildcard(), None, Block(Nil, Literal(UnitConstant())))))).asExprOf[Unit]
  }
}
