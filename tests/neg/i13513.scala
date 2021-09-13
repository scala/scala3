final case class TwoTypes[F, A](value: A)
class Minimal {
  def x[C[_]]: C[Int] = ???
  x[TwoTypes].value // error:  Type argument TwoTypes does not conform to upper bound [_] =>> Any
}
