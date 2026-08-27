trait Expr:
  type Value
object Expr:
  type Of[V] = Expr { type Value = V }
  type ExtractValue[F <: Expr] = F match
    case Expr.Of[v] => v
import Expr.ExtractValue

class SimpleLoop1 extends Expr:
  type Value = ExtractValue[SimpleLoop2]

class SimpleLoop2 extends Expr:
  type Value = ExtractValue[SimpleLoop1]

trait Description:
  type Elem <: Tuple

class PrimBroken extends Expr:
  type Value = Alias // error
  type Alias = Value
