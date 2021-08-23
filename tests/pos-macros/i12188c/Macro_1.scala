import scala.quoted.*

object MatchTest {
  inline def test(a: Int): Unit = ${testImpl('a)}

  def testImpl(a: Expr[Any])(using Quotes): Expr[Unit] = {
    import quotes.reflect.*
    val matchTree = Match(a.asTerm, List(
      CaseDef(Literal(IntConstant(1)), None, Block(Nil, Literal(UnitConstant()))),
      CaseDef(Alternatives(List(Literal(IntConstant(2)), Literal(IntConstant(3)), Literal(IntConstant(4)))), None, Block(Nil, Literal(UnitConstant()))),
      CaseDef(Typed(Wildcard(), TypeIdent(defn.IntClass)), None, Block(Nil, Literal(UnitConstant())))))
    matchTree.asExprOf[Unit]
  }
}