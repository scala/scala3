sealed trait Tupp

case object EmptyTupp extends Tupp
type EmptyTupp = EmptyTupp.type
infix case class `*::`[H, T <: Tupp](h: H, t: T) extends Tupp

type Union[T <: Tupp] = T match
  case EmptyTupp => Nothing
  case h *:: t   => h | Union[t]

type Map[T <: Tupp, F[_ <: Union[T]]] <: Tupp = T match
  case EmptyTupp => EmptyTupp
  case h *:: t   => F[h] *:: Map[t, F]