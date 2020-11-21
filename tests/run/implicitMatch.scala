object Test extends App {
  import collection.immutable.TreeSet
  import collection.immutable.HashSet
  import compiletime.summonInlineOpt

  inline def f1[T]() = inline summonInlineOpt[Ordering[T]] match {
    case Some(ord as given Ordering[T]) => new TreeSet[T]
    case _ => new HashSet[T]
  }

  inline def f2[T]() = inline summonInlineOpt[Ordering[T]] match {
    case Some(given Ordering[T]) => new TreeSet[T]
    case _ => new HashSet[T]
  }

  class A
  class B
  implicit val b: B = new B

  inline def g = inline summonInlineOpt[A] match {
    case _: Some[A] => println("A")
    case _ =>
      inline summonInlineOpt[B] match {
        case _: Some[B] => println("B")
      }
  }

  implicitly[Ordering[String]]

  println(f1[String]().getClass)
  println(f1[AnyRef]().getClass)
  println(f2[String]().getClass)
  println(f2[AnyRef]().getClass)
  implicitly[B]
  g

}