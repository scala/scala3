object Test extends App {
  import collection.immutable.TreeSet
  import collection.immutable.HashSet

  inline def f1[T]() = implied match {
    case ord: Ordering[T] => new TreeSet[T]
    case _ => new HashSet[T]
  }

  inline def f2[T]() = implied match {
    case _: Ordering[T] => new TreeSet[T]
    case _ => new HashSet[T]
  }

  class A
  class B
  implicit val b: B = new B

  inline def g = implied match {
    case _: A => println("A")
    case _: B => println("B")
  }

  implicitly[Ordering[String]]

  println(f1[String]().getClass)
  println(f1[AnyRef]().getClass)
  println(f2[String]().getClass)
  println(f2[AnyRef]().getClass)
  implicitly[B]
  g

}