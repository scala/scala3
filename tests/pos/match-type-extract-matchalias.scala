trait Base:
  type Value
trait Sub[T <: Tuple] extends Base:
  type Value = Tuple.Head[T]
object Base:
  type BaseOf[V] = Base { type Value = V }
  type ExtractValue[B <: Base] = B match
    case BaseOf[v] => v

class Test:
  val test: Base.ExtractValue[Sub[Int *: EmptyTuple]] = 1
