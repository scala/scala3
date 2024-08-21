//> using options -language:experimental.modularity -source future

trait FromString[Self]:
  def fromString(s: String): Self

given FromString[Int] = _.toInt

given FromString[Double] = _.toDouble

def add[N: {FromString, Numeric as num}](a: String, b: String): N =
  N.plus(
    num.plus(N.fromString(a), N.fromString(b)),
    N.fromString(a)
  )