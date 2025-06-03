import scala.quoted.*

inline def listAnnots(inline c: String): List[String] = ${ listAnnotsImpl('c) }

def listAnnotsImpl(c: Expr[String])(using Quotes): Expr[List[String]] =
  import quotes.reflect.*
  Expr(Symbol.requiredClass(c.valueOrError).declaredMethods.flatMap(_.annotations.map(_.show)))
