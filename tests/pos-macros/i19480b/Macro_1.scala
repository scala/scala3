import scala.quoted.*

inline def whatever: Int = ${whateverImpl}

def whateverImpl(using Quotes): Expr[Int] = {
  import quotes.reflect.*
  val t = '{class K[T[_]]}.asTerm
  object mapper extends TreeMap
  mapper.transformTree(t)(Symbol.spliceOwner)
  '{42}
}
