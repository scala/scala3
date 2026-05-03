//> using options -language:experimental.specializedTraits

inline trait Vec[T: {Specialized, Numeric2}](elems: Array[T]):
  private val num = summon[Numeric2[T]]

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
    implicit val v: Numeric2[Int] = new IntIsIntegral() {}
    val x = new Vec[Int](Array(1, 2, 3, 4, 5)) {}
    val y = new Vec[Int](Array(3, 4, 5, 6, 7)) {}
    val z = x.scalarProduct(y)
    assert(z == 85)


inline trait Numeric2[T: Specialized]:
  def fromInt(x: Int): T
  def plus(x: T, y: T): T
  def times(x: T, y: T): T

class IntIsIntegral extends Numeric2[Int]:
  override def fromInt(x: Int): Int = x
  override def plus(x: Int, y: Int): Int = x + y
  override def times(x: Int, y: Int): Int = x * y

