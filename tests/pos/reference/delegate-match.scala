package implicitmatch

class Test extends App {
  import scala.collection.immutable.{TreeSet, HashSet}
  import scala.compiletime.summonFrom

  inline def setFor[T] <: Set[T] = compiletime.summonFrom {
    case given ord: Ordering[T] => new TreeSet[T]
    case _                      => new HashSet[T]
  }

  the[Ordering[String]]

  val sf = setFor[String]

  println(sf.getClass) // prints class scala.collection.immutable.TreeSet

  class A(val x: String)
  implicit val a1: A = new A("a1")
  implicit val a2: A = new A("a2")

  inline def f: Any = summonFrom {
    case ev: A => println(ev.x)  // error: ambiguous implicits
  }
}