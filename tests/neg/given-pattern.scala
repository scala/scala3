

class Test {
  import scala.collection.immutable.{TreeSet, HashSet}

  def f2[T](x: Ordering[T]) = {
    val (given y: Ordering[T]) = x
    new TreeSet[T]    // error: no implicit ordering defined for T
  }
  def f3[T](x: Ordering[T]) = {
    val given y: Ordering[T] = x  // error: pattern expected
    new TreeSet[T]    // error: no implicit ordering defined for T
  }
}