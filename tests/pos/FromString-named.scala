//> using options -language:experimental.modularity -source future

trait FromString[A]:
  def fromString(s: String): A

given FromString[Int] = _.toInt

given FromString[Double] = _.toDouble

def add[N: {FromString as N, Numeric as num}](a: String, b: String): N =
  num.plus(N.fromString(a), N.fromString(b))
