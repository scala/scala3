import scala.quoted.*

def foo[T: Type](expr: Expr[Any])(using Quotes): Any =
  expr match
    case '{ $x: Map[t, t] } =>
    case '{ type t; $x: Any } =>
    case '{ type t; $x: Map[t, t] } =>
    case '{ ($x: Set[t]).toSet[t] } =>

  Type.of[T] match
    case '[Map[t, t]] =>
    case '[(t, t, t, t, t, t, t)] =>
