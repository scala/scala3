object ForSetExample {

  import scala.collection.immutable.*
  import scala.compiletime.summonFrom

  inline def setFor[T]: Set[T] =
    summonFrom {
      case ord: Ordering[T] => new TreeSet[T]
      case _                => new HashSet[T]
    }

  setFor[String] // new TreeSet(scala.math.Ordering.String)
  setFor[Object] // new HashSet

}
