//> using options -language:experimental.specializedTraits

inline trait Vec[T: Specialized](elems: Array[T]):
  def length = elems.length

  def apply(i: Int): T = elems(i)

  def lengthOfOtherVector(other: Vec[Int]): Int = 
    other.length
    
object Test:
  def main(args: Array[String]) = 
    val x = new Vec[Int](Array(1, 2, 3, 4, 5)) {}
    val y = new Vec[Int](Array(3, 4, 5, 6, 7, 9)) {}
    assert(x.lengthOfOtherVector(y) == 6)
    assert(y.lengthOfOtherVector(x) == 5)
