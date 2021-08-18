import scala.quoted.*

inline def annots(inline c: String): List[String] = ${ annotsImpl('c) }

def annotsImpl(c: Expr[String])(using Quotes): Expr[List[String]] =
  import quotes.reflect.*
  // println(c.valueOrError)
  val a = Symbol.requiredClass(c.valueOrError).declaredMethods.map(_.annotations.toString)
  // println(a)
  Expr(a)