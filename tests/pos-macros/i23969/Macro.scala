
import scala.quoted._

object TestMethods:
  def a1 = ()
  def a2() = ()
  def a3[T] = ()
  def a4[T]() = ()

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
  Select.unique('{TestMethods}.asTerm, "a3").ensureApplied match
    case Select(_, _) =>
    case other => assert(false)
  Select.unique('{TestMethods}.asTerm, "a4").ensureApplied match
    case Select(_, _) =>
    case other => assert(false)

  TypeApply(Select.unique('{TestMethods}.asTerm, "a3"), List(TypeTree.of[Nothing])).ensureApplied match
    case TypeApply(_, _) =>
    case other => assert(false)
  TypeApply(Select.unique('{TestMethods}.asTerm, "a4"), List(TypeTree.of[Nothing])).ensureApplied match
    case Apply(_, _) =>
    case other => assert(false)

  // regression test
  val Inlined(_, _, generated) = '{BigDecimal(0).toString()}.asTerm : @unchecked
  val Inlined(_, _, bigDecimal) = '{BigDecimal(0)}.asTerm : @unchecked
  val custom = Select.unique(bigDecimal, "toString").ensureApplied
  // ensure both have the same shape
  assert(custom.toString == generated.toString)
  custom.asExpr
