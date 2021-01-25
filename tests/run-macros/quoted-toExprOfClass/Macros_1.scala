import scala.quoted._

inline def wildcard: Map[String, Class[_]] = ${ wildcardMacro }
inline def noWildcard: Map[String, Class[Int]] = ${ noWildcardMacro }

def wildcardMacro(using Quotes): Expr[Map[String, Class[_]]] = {
  val result: Map[String, Class[_]] = Map(
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
