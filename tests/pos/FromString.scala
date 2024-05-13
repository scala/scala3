//> using options -language:experimental.modularity -source future

trait FromString extends ValueTypeClass:
  def fromString(s: String): Self

given Int is FromString = _.toInt

given Double is FromString = _.toDouble

def add[N: {FromString, Numeric as num}](a: String, b: String): N =
  N.plus(
    num.plus(N.fromString(a), N.fromString(b)),
    N.fromString(a)
  )