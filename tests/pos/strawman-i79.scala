object Empty
  extends App {

  trait Iterable[+A]
      extends IterableOnce[A]

  trait IterableOps[+A, +CC[X], +C] {
    def ++[B >: A](xs: IterableOnce[B]): CC[B] = ???
  }

  trait IterableOnce[+A]

  class LazyList[+A]()
      extends IterableOps[A, LazyList, LazyList[A]]
      with Iterable[A]

  object LazyList {
    def empty[A <: Any]: LazyList[A] = new LazyList[A]()
  }

  LazyList.empty ++ LazyList.empty
}
