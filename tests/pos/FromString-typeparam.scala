//> using options -language:experimental.modularity -source future

trait FromString[A]:
  def fromString(s: String): A

given FromString[Int] = _.toInt

given FromString[Double] = _.toDouble

def add[N: {FromString, Numeric}](a: String, b: String): N =
  val num = summon[Numeric[N]]
  val N = summon[FromString[N]]
  num.plus(N.fromString(a), N.fromString(b))
