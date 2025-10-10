
import scala.quoted._

object TestMethods:
  def a1 = ()
  def a2() = ()

transparent inline def runMacro() = ${runMacroImpl}
def runMacroImpl(using Quotes): Expr[Any] =
  import quotes.reflect._

  // ensureApplied test
  Select.unique('{TestMethods}.asTerm, "a1").ensureApplied match
    case Select(_, _) =>
    case _ => assert(false)
  Select.unique('{TestMethods}.asTerm, "a2").ensureApplied match
    case Apply(_, _) =>
    case _ => assert(false)

  // regression test
  val Inlined(_, _, generated) = '{BigDecimal(0).toString()}.asTerm : @unchecked
  val Inlined(_, _, bigDecimal) = '{BigDecimal(0)}.asTerm : @unchecked
  val custom = Select.unique(bigDecimal, "toString").ensureApplied
  // ensure both have the same shape
  assert(custom.toString == generated.toString)
  custom.asExpr
