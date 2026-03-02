import scala.quoted.*

object TestBuilder:
  inline def test(expr: Unit): Any =
    ${ TestBuilder.processTests('expr) }

  def processTests(using Quotes)(body: Expr[Unit]): Expr[Any] =
    import quotes.reflect.*
    val treeMap = new TreeMap {}
    val mapped =
      treeMap.transformTerm(body.asTerm)(Symbol.spliceOwner)
    mapped.asExpr
