import scala.quoted.*

case class QueryMeta[T](map: Map[String, String])

object QueryMeta:
  given [T]: FromExpr[QueryMeta[T]] = new FromExpr[QueryMeta[T]]:
    def unapply(expr: Expr[QueryMeta[T]])(using q: Quotes): Option[QueryMeta[T]] =
      import q.reflect.*
      expr match
        case '{ QueryMeta/*[T]*/(${ map }: Map[String, String]) } => // error: Reference to T within quotes requires a given scala.quoted.Type[T] in scope.
          map.value.map(QueryMeta[T].apply)
        case _ =>
          None
