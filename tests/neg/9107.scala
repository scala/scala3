trait M[F[_]]
trait Inv[T]

object Test {
  def ev[X] = implicitly[ // error
    (X match { case Inv[t] => Int }) =:=
    (X match { case Inv[t] => t })
  ]

  def ev2[X] = implicitly[ // error
    (M[[t] =>> runtime.MatchCase[Inv[t], Int]])  =:=
    (M[[t] =>> runtime.MatchCase[Inv[t], t]])
  ]
}
