import scala.quoted.*

object Macros {

  inline def fun(x: Any): Unit = ${ impl('x) }

  inline def fun2(x: =>Any): Unit = ${ impl('x) }

  inline def fun3[T]: Unit = ${ impl2(using Type.of[T]) }

  def impl(x: Expr[Any])(using Quotes) : Expr[Unit] = {
    import quotes.reflect.*
    val pos = posStr(x.asTerm.underlyingArgument.pos)
    val code = x.asTerm.underlyingArgument.show
    '{
      println($pos)
      println(${Expr(code)})
    }
  }

  def impl2[T](using x: Type[T])(using Quotes) : Expr[Unit] = {
    import quotes.reflect.*
    val pos = posStr(TypeTree.of[T].pos)
    val code = TypeTree.of[T].show
    '{
      println($pos)
      println(${Expr(code)})
    }
  }

  def posStr(using Quotes)(pos: quotes.reflect.Position): Expr[String] = {
    import quotes.reflect.*
    Expr(s"${pos.sourceFile.name}:[${pos.start}..${pos.end}]")
  }
}
