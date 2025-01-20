import scala.quoted.*

case class QueryMeta[T](map: Map[String, String])

object QueryMeta:
  given [T: Type] => FromExpr[QueryMeta[T]] = new FromExpr[QueryMeta[T]]:
    def unapply(expr: Expr[QueryMeta[T]])(using q: Quotes): Option[QueryMeta[T]] =
      import q.reflect.*
      expr match
        case '{ QueryMeta[t](${ map }: Map[String, String]) } =>
          map.value.map(QueryMeta[T].apply)
        case _ =>
          None
