object Test extends App {
  import collection.immutable.TreeSet
  import collection.immutable.HashSet

  rewrite def f[T]() = implicit match {
    case ord: Ordering[T] => new TreeSet[T]
    case _ => new HashSet[T]
  }

  class A
  class B
  implicit val b: B = new B

  rewrite def g = implicit match {
    case _: A => println("A")
    case _: B => println("B")
  }

  implicitly[Ordering[String]]

  f[String]()
  f[AnyRef]()
  implicitly[B]
  g

}