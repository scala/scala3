package implicitmatch

class Test {
  import scala.collection.immutable.{TreeSet, HashSet}

  inline def setFor[T]: Set[T] = implicit match {
    case ord: Ordering[T] => new TreeSet[T]
    case _                => new HashSet[T]
  }

  the[Ordering[String]]

  println(setFor[String].getClass) // prints class scala.collection.immutable.TreeSet

  class A
  implicit val a1: A = new A
  implicit val a2: A = new A

  inline def f: Any = implicit match {
    case _: A => ???  // error: ambiguous implicits
  }
}