import scala.quoted._

object Macros {

  inline def fun(x: Any): Unit = ${ impl('x) }

  inline def fun2(x: =>Any): Unit = ${ impl('x) }

  inline def fun3[T]: Unit = ${ impl2(using Type[T]) }

  def impl(x: Expr[Any])(using qctx: QuoteContext) : Expr[Unit] = {
    import qctx.reflect._
    val pos = x.unseal.underlyingArgument.pos
    val code = x.unseal.underlyingArgument.show
    '{
      println(${posStr(qctx)(pos)})
      println(${Expr(code)})
    }
  }

  def impl2[T](using x: Type[T])(using qctx: QuoteContext) : Expr[Unit] = {
    import qctx.reflect._
    val pos = TypeTree.of[T].pos
    val code = TypeTree.of[T].show
    '{
      println(${posStr(qctx)(pos)})
      println(${Expr(code)})
    }
  }

  def posStr(qctx: QuoteContext)(pos: qctx.reflect.Position): Expr[String] = {
    given QuoteContext = qctx
    import qctx.reflect._
    Expr(s"${pos.sourceFile.jpath.getFileName.toString}:[${pos.start}..${pos.end}]")
  }
}
