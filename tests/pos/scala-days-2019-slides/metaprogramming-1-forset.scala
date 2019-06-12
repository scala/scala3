object ForSetExample {

  import scala.collection.immutable._

  inline def setFor[T]: Set[T] =
    implicit match {
      case ord: Ordering[T] => new TreeSet[T]
      case _                => new HashSet[T]
    }

  setFor[String] // new TreeSet(scala.math.Ordering.String)
  setFor[Object] // new HashSet

}
