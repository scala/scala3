import scala.quoted._

object Macros {

  inline def fun(x: Any): Unit = ${ impl('x) }

  inline def fun2(x: =>Any): Unit = ${ impl('x) }

  inline def fun3[T]: Unit = ${ impl2(using Type.of[T]) }

  def impl(x: Expr[Any])(using Quotes) : Expr[Unit] = {
    import qctx.reflect._
    val pos = Term.of(x).underlyingArgument.pos
    val code = Term.of(x).underlyingArgument.show
    '{
      println(${posStr(qctx)(pos)})
      println(${Expr(code)})
    }
  }

  def impl2[T](using x: Type[T])(using Quotes) : Expr[Unit] = {
    import qctx.reflect._
    val pos = TypeTree.of[T].pos
    val code = TypeTree.of[T].show
    '{
      println(${posStr(qctx)(pos)})
      println(${Expr(code)})
    }
  }

  def posStr(qctx: Quotes)(pos: qctx.reflect.Position): Expr[String] = {
    given Quotes = qctx
    import qctx.reflect._
    Expr(s"${pos.sourceFile.jpath.getFileName.toString}:[${pos.start}..${pos.end}]")
  }
}
