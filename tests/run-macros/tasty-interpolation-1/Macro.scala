
import scala.quoted._
import scala.language.implicitConversions
import scala.quoted.report.error

object Macro {

  class StringContextOps(strCtx: => StringContext) {
    inline def s2(args: Any*): String = ${SIntepolator('strCtx, 'args)}
    inline def raw2(args: Any*): String = ${RawIntepolator('strCtx, 'args)}
    inline def foo(args: Any*): String = ${FooIntepolator('strCtx, 'args)}
  }
  implicit inline def SCOps(strCtx: => StringContext): StringContextOps = new StringContextOps(strCtx)
}

object SIntepolator extends MacroStringInterpolator[String] {
  protected def interpolate(using s: Scope)(strCtx: StringContext, args: List[s.Expr[Any]]): s.Expr[String] =
    '{(${Expr(strCtx)}).s(${Expr.ofList(args)}: _*)}
}

object RawIntepolator extends MacroStringInterpolator[String] {
  protected def interpolate(using s: Scope)(strCtx: StringContext, args: List[s.Expr[Any]]): s.Expr[String] =
    '{(${Expr(strCtx)}).raw(${Expr.ofList(args)}: _*)}
}

object FooIntepolator extends MacroStringInterpolator[String] {
  protected def interpolate(using s: Scope)(strCtx: StringContext, args: List[s.Expr[Any]]): s.Expr[String] =
    '{(${Expr(strCtx)}).s(${Expr.ofList(args.map(_ => '{"foo"}))}: _*)}
}

// TODO put this class in the stdlib or separate project?
abstract class MacroStringInterpolator[T] {

  final def apply(using s: Scope)(strCtxExpr: s.Expr[StringContext], argsExpr: s.Expr[Seq[Any]]): s.Expr[T] = {
    try interpolate0(strCtxExpr, argsExpr)
    catch {
      case ex: NotStaticlyKnownError =>
        // TODO use ex.expr to recover the position
        error(ex.getMessage)
        '{???}
      case ex: StringContextError =>
        // TODO use ex.idx to recover the position
        error(ex.getMessage)
        '{???}
      case ex: ArgumentError =>
        // TODO use ex.idx to recover the position
        error(ex.getMessage)
        '{???}
    }
  }

  protected def interpolate0(using s: Scope)(strCtxExpr: s.Expr[StringContext], argsExpr: s.Expr[Seq[Any]]): s.Expr[T] =
    interpolate(getStaticStringContext(strCtxExpr), getArgsList(argsExpr))

  protected def interpolate(using s: Scope)(strCtx: StringContext, argExprs: List[s.Expr[Any]]): s.Expr[T]

  protected def getStaticStringContext(using s: Scope)(strCtxExpr: s.Expr[StringContext]): StringContext = {
    import s.tasty._
    strCtxExpr.underlyingArgument match {
      case Select(Typed(Apply(_, List(Apply(_, List(Typed(Repeated(strCtxArgTrees, _), Inferred()))))), _), _) =>
        val strCtxArgs = strCtxArgTrees.map {
          case Literal(Constant(str: String)) => str
          case tree => throw new NotStaticlyKnownError("Expected statically known StringContext")
        }
        StringContext(strCtxArgs: _*)
      case tree =>
        throw new NotStaticlyKnownError("Expected statically known StringContext")
    }
  }

  protected def getArgsList(using s: Scope)(argsExpr: s.Expr[Seq[Any]]): List[s.Expr[Any]] = {
    import s.tasty._
    argsExpr.underlyingArgument match {
      case Typed(Repeated(args, _), _) => args.map(_.seal)
      case tree => throw new NotStaticlyKnownError("Expected statically known argument list")
    }
  }

  protected implicit def StringContextIsLiftable(using s: Scope): s.Liftable[StringContext] = new s.Liftable[StringContext] {
    def toExpr(strCtx: StringContext) = '{StringContext(${s.Expr(strCtx.parts.toSeq)}: _*)}
  }

  protected class NotStaticlyKnownError(msg: String) extends Exception(msg)
  protected class StringContextError(msg: String, idx: Int, start: Int = -1, end: Int = -1) extends Exception(msg)
  protected class ArgumentError(msg: String, idx: Int) extends Exception(msg)

}
