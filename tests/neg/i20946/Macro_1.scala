import scala.quoted.*

def macroWithAssertFailingImpl[T: Type](t: Expr[T])(using Quotes): Expr[Unit] = {
  import quotes.reflect.*

  Ref(TypeRepr.of[T].typeSymbol)

  '{()}
}

