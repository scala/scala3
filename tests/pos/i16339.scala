// scalac: -Werror
sealed trait Get[X, +X2 <: X]
case class Bar[Y, Y2 <: Y](value: Y2) extends Get[Y, Y2]


class Test:
  def t1[Z, Z2 <: Z](get: Get[Z, Z2]) = get match
    case Bar(_) =>
