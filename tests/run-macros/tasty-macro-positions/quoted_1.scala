import scala.quoted._

object Macros {

  inline def fun(x: Any): Unit = ${ impl('x) }

  inline def fun2(x: =>Any): Unit = ${ impl('x) }

  inline def fun3[T]: Unit = ${ impl2('[T]) }

  def impl(x: Expr[Any]) given (qctx: QuoteContext): Expr[Unit] = {
    import qctx.tasty._
    val pos = x.unseal.underlyingArgument.pos
    val code = x.unseal.underlyingArgument.show
    '{
      println(${posStr(qctx)(pos)})
      println(${code.toExpr})
    }
  }

  def impl2[T](x: quoted.Type[T]) given (qctx: QuoteContext): Expr[Unit] = {
    import qctx.tasty._
    val pos = x.unseal.pos
    val code = x.unseal.show
    '{
      println(${posStr(qctx)(pos)})
      println(${code.toExpr})
    }
  }

  def posStr(qctx: QuoteContext)(pos: qctx.tasty.Position): Expr[String] = {
    delegate for QuoteContext = qctx
    import qctx.tasty._
    s"${pos.sourceFile.jpath.getFileName.toString}:[${pos.start}..${pos.end}]".toExpr
  }
}
