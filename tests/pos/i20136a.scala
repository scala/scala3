
trait Expr:
  type Value
object Expr:
  type Of[V] = Expr { type Value = V }
  type ExtractValue[E <: Expr] = E match
    case Expr.Of[v] => v

trait TC[E <: Expr]:
  type Elem = Expr.ExtractValue[E]
class BIExpr extends Expr:
  type Value = BigInt
class Foo extends TC[BIExpr]:
  val v: Elem = 0
