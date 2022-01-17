
import scala.quoted.*
import scala.language.implicitConversions

object Macro {

  class StringContextOps(strCtx: => StringContext) {
    inline def s2(args: Any*): String = ${SIntepolator('strCtx, 'args)}
    inline def raw2(args: Any*): String = ${RawIntepolator('strCtx, 'args)}
    inline def foo(args: Any*): String = ${FooIntepolator('strCtx, 'args)}
  }
  implicit inline def SCOps(strCtx: => StringContext): StringContextOps = new StringContextOps(strCtx)
}

object SIntepolator extends MacroStringInterpolator[String] {
  protected def interpolate(strCtx: StringContext, args: List[Expr[Any]]) (using Quotes): Expr[String] =
    '{(${Expr(strCtx)}).s(${Expr.ofList(args)}*)}
}

object RawIntepolator extends MacroStringInterpolator[String] {
  protected def interpolate(strCtx: StringContext, args: List[Expr[Any]]) (using Quotes): Expr[String] =
    '{(${Expr(strCtx)}).raw(${Expr.ofList(args)}*)}
}

object FooIntepolator extends MacroStringInterpolator[String] {
  protected def interpolate(strCtx: StringContext, args: List[Expr[Any]]) (using Quotes): Expr[String] =
    '{(${Expr(strCtx)}).s(${Expr.ofList(args.map(_ => '{"foo"}))}*)}
}

// TODO put this class in the stdlib or separate project?
abstract class MacroStringInterpolator[T] {

  final def apply(strCtxExpr: Expr[StringContext], argsExpr: Expr[Seq[Any]])(using Quotes) : Expr[T] = {
    import quotes.reflect.report.error
    try interpolate(strCtxExpr, argsExpr)
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

  protected def interpolate(strCtxExpr: Expr[StringContext], argsExpr: Expr[Seq[Any]]) (using Quotes): Expr[T] =
    interpolate(getStaticStringContext(strCtxExpr), getArgsList(argsExpr))

  protected def interpolate(strCtx: StringContext, argExprs: List[Expr[Any]]) (using Quotes): Expr[T]

  protected def getStaticStringContext(strCtxExpr: Expr[StringContext])(using Quotes) : StringContext = {
    import quotes.reflect.*
    strCtxExpr.asTerm.underlyingArgument match {
      case Apply(Select(Select(Typed(Apply(_, List(Apply(Ident("<byname>"), List(Apply(Select(Select(Select(Ident("_root_"), "scala"), "StringContext"), "apply"), List(Typed(Repeated(strCtxArgTrees, _), Inferred()))))))), _), _), _), Nil) =>
        val strCtxArgs = strCtxArgTrees.map {
          case Literal(StringConstant(str)) => str
          case tree => throw new NotStaticlyKnownError("Expected statically known StringContext", tree.asExpr)
        }
        StringContext(strCtxArgs*)
      // Old style by-name
      case Select(Typed(Apply(_, List(Apply(_, List(Typed(Repeated(strCtxArgTrees, _), Inferred()))))), _), _) =>
        val strCtxArgs = strCtxArgTrees.map {
          case Literal(StringConstant(str)) => str
          case tree => throw new NotStaticlyKnownError("Expected statically known StringContext", tree.asExpr)
        }
        StringContext(strCtxArgs*)
      case tree =>
        throw new NotStaticlyKnownError("Expected statically known StringContext", tree.asExpr)
    }
  }

  protected def getArgsList(argsExpr: Expr[Seq[Any]])(using Quotes) : List[Expr[Any]] = {
    import quotes.reflect.*
    argsExpr.asTerm.underlyingArgument match {
      case Typed(Repeated(args, _), _) => args.map(_.asExpr)
      case tree => throw new NotStaticlyKnownError("Expected statically known argument list", tree.asExpr)
    }
  }

  protected implicit def StringContextIsToExpr: ToExpr[StringContext] = new ToExpr[StringContext] {
    def apply(strCtx: StringContext)(using Quotes) = '{StringContext(${Expr(strCtx.parts.toSeq)}*)}
  }

  protected class NotStaticlyKnownError(msg: String, expr: Expr[Any]) extends Exception(msg)
  protected class StringContextError(msg: String, idx: Int, start: Int = -1, end: Int = -1) extends Exception(msg)
  protected class ArgumentError(msg: String, idx: Int) extends Exception(msg)

}
