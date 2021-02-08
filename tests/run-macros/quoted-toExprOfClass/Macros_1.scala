import scala.quoted.*

inline def wildcard: Map[String, Class[?]] = ${ wildcardMacro }
inline def noWildcard: Map[String, Class[Int]] = ${ noWildcardMacro }

def wildcardMacro(using Quotes): Expr[Map[String, Class[?]]] = {
  val result: Map[String, Class[?]] = Map(
    "foo" -> classOf[Long],
    "bar" -> classOf[Int]
  )
  Expr(result)
}

def noWildcardMacro(using Quotes): Expr[Map[String, Class[Int]]] = {
  val result: Map[String, Class[Int]] = Map(
    "foo" -> classOf[Int],
    "bar" -> classOf[Int]
  )
  Expr(result)
}
