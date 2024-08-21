package a

trait Expr:
  type Value
object Expr:
  type Of[V] = Expr { type Value = V }
  type ExtractValue[E <: Expr] = E match
    case Expr.Of[v] => v
