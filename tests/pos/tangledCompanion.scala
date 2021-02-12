object Test {

  import Coll.*
  import LazyList.#::

  val xs = LazyList.Empty

}

object Coll {

  trait IterableFactory[+C[X]]

  class LazyList[+A](expr: => LazyList.Evaluated[A])

  object LazyList extends IterableFactory[LazyList] {

    type Evaluated[+A] = Option[(A, LazyList[A])]

    object Empty extends LazyList[Nothing](None)

    object #:: {
      def unapply[A](s: LazyList[A]): Evaluated[A] = ???
    }
  }
}
