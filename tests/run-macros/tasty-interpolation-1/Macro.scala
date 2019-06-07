
import scala.quoted._
import scala.tasty.Reflection
import scala.language.implicitConversions
import scala.quoted.autolift._

object Macro {

  class StringContextOps(strCtx: => StringContext) {
    inline def s2(args: Any*): String = ${SIntepolator('strCtx, 'args)}
    inline def raw2(args: Any*): String = ${RawIntepolator('strCtx, 'args)}
    inline def foo(args: Any*): String = ${FooIntepolator('strCtx, 'args)}
  }
  implicit inline def SCOps(strCtx: => StringContext): StringContextOps = new StringContextOps(strCtx)
}

object SIntepolator extends MacroStringInterpolator[String] {
  protected def interpolate(strCtx: StringContext, args: List[Expr[Any]])(implicit reflect: Reflection): Expr[String] =
    '{(${strCtx}).s(${args.toExprOfList}: _*)}
}

object RawIntepolator extends MacroStringInterpolator[String] {
  protected def interpolate(strCtx: StringContext, args: List[Expr[Any]])(implicit reflect: Reflection): Expr[String] =
    '{(${strCtx}).raw(${args.toExprOfList}: _*)}
}

object FooIntepolator extends MacroStringInterpolator[String] {
  protected def interpolate(strCtx: StringContext, args: List[Expr[Any]])(implicit reflect: Reflection): Expr[String] =
    '{(${strCtx}).s(${args.map(_ => '{"foo"}).toExprOfList}: _*)}
}

// TODO put this class in the stdlib or separate project?
abstract class MacroStringInterpolator[T] {

  final def apply(strCtxExpr: Expr[StringContext], argsExpr: Expr[Seq[Any]])(implicit reflect: Reflection): Expr[T] = {
    try interpolate(strCtxExpr, argsExpr)
    catch {
      case ex: NotStaticlyKnownError =>
        // TODO use ex.expr to recover the position
        QuoteError(ex.getMessage)
      case ex: StringContextError =>
        // TODO use ex.idx to recover the position
        QuoteError(ex.getMessage)
      case ex: ArgumentError =>
        // TODO use ex.idx to recover the position
        QuoteError(ex.getMessage)
    }
  }

  protected def interpolate(strCtxExpr: Expr[StringContext], argsExpr: Expr[Seq[Any]])(implicit reflect: Reflection): Expr[T] =
    interpolate(getStaticStringContext(strCtxExpr), getArgsList(argsExpr))

  protected def interpolate(strCtx: StringContext, argExprs: List[Expr[Any]])(implicit reflect: Reflection): Expr[T]

  protected def getStaticStringContext(strCtxExpr: Expr[StringContext])(implicit reflect: Reflection): StringContext = {
    import reflect._
    strCtxExpr.unseal.underlyingArgument match {
      case Select(Typed(Apply(_, List(Apply(_, List(Typed(Repeated(strCtxArgTrees, _), Inferred()))))), _), _) =>
        val strCtxArgs = strCtxArgTrees.map {
          case Literal(Constant.String(str)) => str
          case tree => throw new NotStaticlyKnownError("Expected statically known StringContext", tree.seal)
        }
        StringContext(strCtxArgs: _*)
      case tree =>
        throw new NotStaticlyKnownError("Expected statically known StringContext", tree.seal)
    }
  }

  protected def getArgsList(argsExpr: Expr[Seq[Any]])(implicit reflect: Reflection): List[Expr[Any]] = {
    import reflect._
    argsExpr.unseal.underlyingArgument match {
      case Typed(Repeated(args, _), _) => args.map(_.seal)
      case tree => throw new NotStaticlyKnownError("Expected statically known argument list", tree.seal)
    }
  }

  protected implicit def StringContextIsLiftable: Liftable[StringContext] = new Liftable[StringContext] {
    def toExpr(strCtx: StringContext): Expr[StringContext] = {
      // TODO define in stdlib?
      implicit def ListIsLiftable: Liftable[List[String]] = new Liftable[List[String]] {
        override def toExpr(list: List[String]): Expr[List[String]] = list match {
          case x :: xs => '{${x.toExpr} :: ${toExpr(xs)}}
          case Nil => '{Nil}
        }
      }
      '{StringContext(${strCtx.parts.toList}: _*)}
    }
  }

  protected class NotStaticlyKnownError(msg: String, expr: Expr[Any]) extends Exception(msg)
  protected class StringContextError(msg: String, idx: Int, start: Int = -1, end: Int = -1) extends Exception(msg)
  protected class ArgumentError(msg: String, idx: Int) extends Exception(msg)

}
