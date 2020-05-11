import scala.quoted._

object Macros {

  inline def fun(x: Any): Unit = ${ impl('x) }

  inline def fun2(x: =>Any): Unit = ${ impl('x) }

  inline def fun3[T]: Unit = ${ impl2('[T]) }

  def impl(using s: Scope)(x: s.Expr[Any]): s.Expr[Unit] = {
    import s.tasty._
    val pos = x.underlyingArgument.pos
    val code = x.underlyingArgument.show
    '{
      println(${posStr(s)(pos)})
      println(${s.Expr(code)})
    }
  }

  def impl2[T](using s: Scope)(x: s.Type[T]) : s.Expr[Unit] = {
    import s.tasty._
    val pos = x.pos
    val code = x.show
    '{
      println(${posStr(s)(pos)})
      println(${s.Expr(code)})
    }
  }

  def posStr(s: Scope)(pos: s.tasty.Position): s.Expr[String] = {
    given s.type = s
    import s.tasty._
    s.Expr(s"${pos.sourceFile.jpath.getFileName.toString}:[${pos.start}..${pos.end}]")
  }
}
