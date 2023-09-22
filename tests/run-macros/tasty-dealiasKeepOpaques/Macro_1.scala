import scala.quoted.*

inline def dealiasKeepOpaques[T]: String = ${ impl[T] }

def impl[T: Type](using Quotes) : Expr[String] = {
  import quotes.reflect.*
  Expr(TypeRepr.of[T].dealiasKeepOpaques.show)
}
