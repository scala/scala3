trait M[F[_]]
trait Inv[T]

object Test {
  def ev[X] = implicitly[
    (X match { case Inv[t] => Int }) =:=
    (X match { case Inv[t] => t })
  ] // error

  def ev2[X] = implicitly[
    (M[[t] =>> runtime.MatchCase[Inv[t], Int]])  =:=
    (M[[t] =>> runtime.MatchCase[Inv[t], t]])
  ] // error
}
