import scala.quoted._
import scala.compiletime.testing.{typeChecks, typeCheckErrors}

trait Assertion
trait Bool {
  def value: Boolean
}
class SimpleMacroBool(expression: Boolean) extends Bool {
  override def value: Boolean = expression
}
class BinaryMacroBool(left: Any, operator: String, right: Any, expression: Boolean) extends Bool {
  override def value: Boolean = expression
}
object Bool {
  def simpleMacroBool(expression: Boolean): Bool = new SimpleMacroBool(expression)
  def binaryMacroBool(left: Any, operator: String, right: Any, expression: Boolean): Bool =
    new BinaryMacroBool(left, operator, right, expression)
  def binaryMacroBool(left: Any, operator: String, right: Any, bool: Bool): Bool =
    new BinaryMacroBool(left, operator, right, bool.value)
}

object Assertions {
  inline def assert(inline condition: Boolean): Assertion =
    ${ AssertionsMacro.assert('{ condition }) }
}

object AssertionsMacro {
  def assert(condition: Expr[Boolean])(using Quotes): Expr[Assertion] =
    transform(condition)

  def transform(
      condition: Expr[Boolean]
  )(using Quotes): Expr[Assertion] = {
    val bool = BooleanMacro.parse(condition)
    '{
      new Assertion {
        val condition = $bool
      }
  }
  }
}

object BooleanMacro {
  private val supportedBinaryOperations =
    Set("!=", "==")

  def parse(condition: Expr[Boolean])(using Quotes): Expr[Bool] = {
    import quotes.reflect._
    import quotes.reflect.ValDef.let
    import util._

    def exprStr: String = condition.show
    def defaultCase = '{ Bool.simpleMacroBool($condition) }

    def isByNameMethodType(tp: TypeRepr): Boolean = tp.widen match {
      case MethodType(_, ByNameType(_) :: Nil, _) => true
      case _                                      => false
    }

    condition.asTerm.underlyingArgument match { // WARNING: unsound use of `underlyingArgument`
      case Apply(sel @ Select(lhs, op), rhs :: Nil) =>
        def binaryDefault =
          if (isByNameMethodType(sel.tpe)) defaultCase
          else if (supportedBinaryOperations.contains(op)) {
            let(Symbol.spliceOwner, lhs) { left =>
              let(Symbol.spliceOwner, rhs) { right =>
                val app = left.select(sel.symbol).appliedTo(right)
                let(Symbol.spliceOwner, app) { result =>
                  val l = left.asExpr
                  val r = right.asExpr
                  val b = result.asExprOf[Boolean]
                  val code = '{ Bool.binaryMacroBool($l, ${ Expr(op) }, $r, $b) }
                  code.asTerm
                }
              }
            }.asExprOf[Bool]
          } else defaultCase

        op match {
          case "==" => binaryDefault
          case _    => binaryDefault
        }

      case Literal(_) =>
        '{ Bool.simpleMacroBool($condition) }

      case _ =>
        defaultCase
    }
  }
}
