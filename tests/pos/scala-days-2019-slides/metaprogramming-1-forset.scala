object ForSetExample {

  import scala.collection.immutable._
  import scala.compiletime.summonInlineOpt

  inline def setFor[T]: Set[T] =
    inline summonInlineOpt[Ordering[T]] match {
      case Some(given _) => new TreeSet[T]
      case _             => new HashSet[T]
    }

  setFor[String] // new TreeSet(scala.math.Ordering.String)
  setFor[Object] // new HashSet

}
