import scala.quoted.*

inline def whatever: Int = ${whateverImpl}

def whateverImpl(using Quotes): Expr[Int] = {
  import quotes.reflect.*
  val t = '{class K[T]}.asTerm
  object mapper extends TreeMap
  mapper.transformTree(t)(Symbol.spliceOwner)
  '{42}
}
