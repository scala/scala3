import scala.quoted.*

inline def annots(inline c: String): List[String] = ${ annotsImpl('c) }

def annotsImpl(c: Expr[String])(using Quotes): Expr[List[String]] =
  import quotes.reflect.*
  Expr(Symbol.requiredClass(c.valueOrError).declaredMethods.map(_.annotations.toString))