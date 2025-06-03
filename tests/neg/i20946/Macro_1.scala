import scala.quoted.*

def macroWithAssertFailingImpl[T: Type](t: Expr[T])(using Quotes): Expr[Unit] = {
  import quotes.reflect.*

  try
    Ref(TypeRepr.of[T].typeSymbol)
  catch
    case ex: Throwable =>
      if ex.getMessage().contains("expected a term symbol, but received ") then
        throw ex

  '{()}
}
