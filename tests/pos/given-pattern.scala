

class Test {
  import scala.collection.immutable.{TreeSet, HashSet}

  transparent inline def trySummon[S, T](f: PartialFunction[S, T]): T = ???

  inline def setFor[T]: Set[T] = trySummon {
    case ord @ given Ordering[T] => new TreeSet[T]
    case given Ordering[T]       => new TreeSet[T]
    case _                       => new HashSet[T]
  }

  def f1[T](x: Ordering[T]) = (x, x) match {
    case (y @ given Ordering[T], _) => new TreeSet[T]
  }
  def f2[T](x: Ordering[T]) = {
    val xs = List(x, x, x)
    for y @ given Ordering[T] <- xs
    yield new TreeSet[T]
  }
  def f3[T](x: Ordering[T]) = (x, x) match {
    case (given Ordering[T], _) => new TreeSet[T]
  }
  def f4[T](x: Ordering[T]) = {
    val xs = List(x, x, x)
    for given Ordering[T] <- xs
    yield new TreeSet[T]
  }
}