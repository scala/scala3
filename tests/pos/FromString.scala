//> using options -language:experimental.modularity -source future

trait FromString:
  type Self
  def fromString(s: String): Self

given Int forms FromString = _.toInt

given Double forms FromString = _.toDouble

def add[N: {FromString as fs, Numeric as num}](a: String, b: String): N =
  num.plus(fs.fromString(a), fs.fromString(b))
