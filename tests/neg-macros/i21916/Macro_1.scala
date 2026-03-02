import scala.quoted._
object Macro:
  inline def test() = ${testImpl}
  def testImpl(using Quotes): Expr[Any] = {
    import quotes.reflect._
    val tpe = TypeRepr.of[Array[Byte]] match
      case AppliedType(tycons, _) => tycons
    Literal(ClassOfConstant(tpe)).asExpr
  }
