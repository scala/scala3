sealed trait SUB[A, +B]
final case class Refl[T]() extends SUB[T, T]

class Test:
  def t1[A, B, C](sub: SUB[A | B, C]) = sub match
    case Refl() => // C >: A | B
