case class Box[T](t: T)

type Boxed[T <: Tuple] <: Tuple = T match {
  case EmptyTuple => EmptyTuple
  case h *: t => Box[h] *: Boxed[t]
}

trait Cmp[T <: Tuple] { def cmp(t: T, b: Boxed[T]): Boolean }

object UnitCmp extends Cmp[EmptyTuple] {
  def cmp(t: EmptyTuple, b: EmptyTuple): Boolean = true
}

object UnitCmp2 extends Cmp[EmptyTuple] {
  def cmp(t: EmptyTuple, b: Boxed[EmptyTuple]): Boolean = true
}
