
type OldFilter[Tup <: Tuple, P[_] <: Boolean] = Nothing
type NewFilter[Tup <: Tuple, P[_ <: Tuple.Union[Tup]] <: Boolean] = Nothing

trait A:
  type X >: OldFilter <: OldFilter

trait B1 extends A:
  type X = OldFilter // ok

trait B2 extends A:
  type X = NewFilter // error: breaking change
