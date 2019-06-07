import scala.quoted._

import scala.tasty._

object Macros {

  inline def fun(x: Any): Unit = ${ impl('x) }

  inline def fun2(x: =>Any): Unit = ${ impl('x) }

  inline def fun3[T]: Unit = ${ impl2('[T]) }

  def impl(x: Expr[Any])(implicit reflect: Reflection): Expr[Unit] = {
    import reflect._
    val pos = x.unseal.underlyingArgument.pos
    val code = x.unseal.underlyingArgument.show
    '{
      println(${posStr(reflect)(pos)})
      println(${code.toExpr})
    }
  }

  def impl2[T](x: quoted.Type[T])(implicit reflect: Reflection): Expr[Unit] = {
    import reflect._
    val pos = x.unseal.pos
    val code = x.unseal.show
    '{
      println(${posStr(reflect)(pos)})
      println(${code.toExpr})
    }
  }

  def posStr(relfection: Reflection)(pos: relfection.Position): Expr[String] = {
    import relfection._
    s"${pos.sourceFile.jpath.getFileName.toString}:[${pos.start}..${pos.end}]".toExpr
  }
}
