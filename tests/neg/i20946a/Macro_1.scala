import scala.quoted.*

def macroWithAssertFailingImpl[T: Type](t: Expr[T])(using Quotes): Expr[Unit] = {
  import quotes.reflect.*

  try
    TypeIdent(t.asTerm.symbol)
  catch
    case ex: Throwable =>
      if ex.getMessage().contains("Expected a type symbol, but got ") then
        throw ex

  '{()}
}
