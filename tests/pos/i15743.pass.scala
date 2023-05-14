// Like pos/i15743 but already passed, because the bounds are never lost so reduction never fails
class Pass:
  def pass[T >: Tuple <: Tuple](t2: Int *: T) =
    val i1: Int = (t2: Int *: T).head[Int *: T]
