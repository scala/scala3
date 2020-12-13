import math.Ordering
import collection.immutable.TreeSet

def f1[T](x: Ordering[T]) = (x, x) match {
  case (given Ordering[T], _) => new TreeSet[T]
}
def f4[T](x: Ordering[T]) = {
  val xs = List(x, x, x)
  for given Ordering[T] <- xs
  yield new TreeSet[T]
  for x @ given Ordering[T] <- xs
  yield new TreeSet[T]
}