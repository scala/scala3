import scala.Tuple.*

trait Trait1
trait Trait2

case class Box[+T](t: T)

type N[x] = x match {
  case Box[String] => Trait1
  case Box[Int] => Trait2
}

trait Cov[+T]
type M[t] = t match {
  case Cov[x] => N[x]
}

trait Root[A] {
  def thing: M[A]
}

class Asploder extends Root[Cov[Box[Int & String]]] {
  def thing = new Trait1 {} // error
  //                       ^
  //                   Found:    Object with Trait1 {...}
  //                   Required: N[Box[Int & String]]
  //
  //                   Note: a match type could not be fully reduced:
  //
  //                     trying to reduce  N[Box[Int & String]]
  //                     failed since selector  Box[Int & String]
  //                     is uninhabited (there are no values of that type).
}

object Main {
  def foo[T <: Cov[Box[Int]]](c: Root[T]): Trait2 = c.thing
  def explode = foo(new Asploder)

  def main(args: Array[String]): Unit =
    explode
}
