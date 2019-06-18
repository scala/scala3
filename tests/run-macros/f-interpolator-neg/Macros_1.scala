import scala.quoted._
import scala.quoted.autolift._
import scala.quoted.matching._
import scala.tasty.Reflection

import scala.language.implicitConversions

object TestFooErrors { // Defined in tests
  implicit object StringContextOps {
    inline def (ctx: => StringContext) foo (args: => Any*): List[(Boolean, Int, Int, Int, String)] = ${ Macro.fooErrors('ctx, 'args) }
  }
}

object Macro {

  def fooErrors(strCtxExpr: Expr[StringContext], argsExpr: Expr[Seq[Any]]) given (reflect: Reflection): Expr[List[(Boolean, Int, Int, Int, String)]] = {
    (strCtxExpr, argsExpr) match {
      case ('{ StringContext(${ExprSeq(parts)}: _*) }, ExprSeq(args)) =>
        fooErrorsImpl(parts, args, argsExpr)
    case ('{ new StringContext(${ExprSeq(parts)}: _*) }, ExprSeq(args)) =>
      fooErrorsImpl(parts, args, argsExpr)
    }
  }

  def fooErrorsImpl(parts : Seq[Expr[String]], args: Seq[Expr[Any]], argsExpr: Expr[Seq[Any]]) given (reflect: Reflection)= {
    val errors = List.newBuilder[Expr[(Boolean, Int, Int, Int, String)]]
    // true if error, false if warning
    // 0 if part, 1 if arg, 2 if strCtx, 3 if args
    // index in the list if arg or part, -1 otherwise
    // offset, 0 if strCtx, args or arg
    // message as given
    val reporter = new dotty.internal.StringContextMacro.Reporter{
      private[this] var reported = false
      private[this] var oldReported = false
      def partError(message : String, index : Int, offset : Int) : Unit = {
        reported = true
        errors += '{ Tuple5(true, 0, $index, $offset, $message) }
      }
      def partWarning(message : String, index : Int, offset : Int) : Unit = {
        reported = true
        errors += '{ Tuple5(false, 0, $index, $offset, $message) }
      }

      def argError(message : String, index : Int) : Unit = {
        reported = true
        errors += '{ Tuple5(true, 1, $index, 0, $message) }
      }

      def strCtxError(message : String) : Unit = {
        reported = true
        errors += '{ Tuple5(true, 2, -1, 0, $message) }
      }
      def argsError(message : String) : Unit = {
        reported = true
        errors += '{ Tuple5(true, 3, -1, 0, $message) }
      }

      def hasReported() : Boolean = {
        reported
      }

      def resetReported() : Unit = {
        oldReported = reported
        reported = false
      }

      def restoreReported() : Unit = {
        reported = oldReported
      }
    }
    dotty.internal.StringContextMacro.interpolate(parts.toList, args.toList, argsExpr, reporter) // Discard result
    errors.result().toExprOfList
  }
}