import scala.quoted.*

sealed class Foo()
inline def hh(): Unit = ${ interpMacro() }

private def interpMacro()(using Quotes): Expr[Unit] =
  import quotes.reflect.*
  '{
    val res: Either[String, (Foo, Foo)] =
      Right((new Foo, new Foo))
    val (a, b) = res.toOption.get
  }
