class Rational(p: Int, q: Int with q != 0):
  val asFloat = p / q

val r = Rational(1, 2).asFloat
