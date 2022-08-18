trait Tag { val data: Int }

enum EQ[A, B]:
  case Refl[C]() extends EQ[C, C]

def foo[T, B <: Tag](ev: EQ[T, B], x: T) = ev match
  case EQ.Refl() =>
    val i: Int = x.data

