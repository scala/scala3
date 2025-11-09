sealed trait Tup
case object Emp extends Tup
type Emp = Emp.type
case class Cons[h, t <: Tup](hh: h, tt: t) extends Tup

type Union[T <: Tup] = T match
  case Emp        => Nothing
  case Cons[h, t] => h | Union[t]

type Concat[T <: Tup, U <: Tup] <: Tup = T match
  case Emp        => U
  case Cons[h, t] => Cons[h, Concat[t, U]]

type FlatMap[T <: Tup, F[_ <: Union[T]] <: Tup] <: Tup = T match
  case Emp        => Emp
  case Cons[h, t] => Concat[F[h], FlatMap[t, F]]

type A =
  FlatMap[Cons[Boolean, Cons[String, Emp]], [T] =>> Cons[T, Cons[List[T], Emp]]]