import scala.quoted._


import scala.language.implicitConversions

object TestFooErrors { // Defined in tests
  implicit object StringContextOps {
    extension (inline ctx: StringContext) inline def foo(inline args: Any*): List[(Boolean, Int, Int, Int, String)] = ${ Macro.fooErrors('ctx, 'args) }
  }
}

object Macro {

  def fooErrors(using s: Scope)(strCtxExpr: s.Expr[StringContext], argsExpr: s.Expr[Seq[Any]]): s.Expr[List[(Boolean, Int, Int, Int, String)]] = {
    (strCtxExpr, argsExpr) match {
      case ('{ StringContext(${Varargs(parts)}: _*) }, Varargs(args)) =>
        fooErrorsImpl(parts, args, argsExpr)
    case ('{ new StringContext(${Varargs(parts)}: _*) }, Varargs(args)) =>
      fooErrorsImpl(parts, args, argsExpr)
    }
  }

  def fooErrorsImpl(using s: Scope)(parts0: Seq[s.Expr[String]], args: Seq[s.Expr[Any]], argsExpr: s.Expr[Seq[Any]]) = {
    val errors = List.newBuilder[s.Expr[(Boolean, Int, Int, Int, String)]]
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
        errors += '{ Tuple5(true, 0, ${Expr(index)}, ${Expr(offset)}, ${Expr(message)}) }
      }
      def partWarning(message : String, index : Int, offset : Int) : Unit = {
        reported = true
        errors += '{ Tuple5(false, 0, ${Expr(index)}, ${Expr(offset)}, ${Expr(message)}) }
      }

      def argError(message : String, index : Int) : Unit = {
        reported = true
        errors += '{ Tuple5(true, 1, ${Expr(index)}, 0, ${Expr(message)}) }
      }

      def strCtxError(message : String) : Unit = {
        reported = true
        errors += '{ Tuple5(true, 2, -1, 0, ${Expr(message)}) }
      }
      def argsError(message : String) : Unit = {
        reported = true
        errors += '{ Tuple5(true, 3, -1, 0, ${Expr(message)}) }
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
    val parts = parts0.map { case Const(s) => s }
    dotty.internal.StringContextMacro.interpolate2(parts.toList, args.toList, argsExpr, reporter) // Discard result
    Expr.ofList(errors.result())
  }
}