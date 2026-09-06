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

object Test1:
  val x: ExtractValue[SimpleLoop1] = 1 // error
