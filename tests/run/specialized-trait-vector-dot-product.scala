//> using options -language:experimental.specializedTraits

inline trait Vec[T: {Specialized, Numeric}](elems: Array[T]):
  private val num = summon[Numeric[T]]

  def length = elems.length

  def apply(i: Int): T = elems(i)

  def scalarProduct(other: Vec[T]): T =
    require(this.length == other.length)
    var result = num.fromInt(0)
    for i <- 0 until length do
      result = num.plus(result, num.times(this(i), other(i)))
    result

object Test:
  def main(args: Array[String]) = 
    val x = new Vec[Int](Array(1, 2, 3, 4, 5)) {}
    val y = new Vec[Int](Array(3, 4, 5, 6, 7)) {}
    val z = x.scalarProduct(y)
    assert(z == 85)
