object TestList {

  var x: ([X] =>> List[List[X]])[_] = List(List(1)) // error: unreducible
  var y: ([X] =>> List[Seq[X]])[_] = List(List(1)) // error: unreducible

  x = x
  y = y
  y = x

  val h = x.head
  val x1: List[_] = h

  var z: List[_] = x

}
object TestSet {

  var x: ([Y] =>> Set[Set[Y]])[_] = Set(Set("a")) // error: unreducible
  var y: ([Y] =>> Set[Iterable[Y]])[_] = Set(Set("a")) // error: unreducible

  x = x
  y = y

  val h = x.head
  val h1: Set[_] = h

  // val p = x.+ // infinite loop in implicit search

  var z: Set[_] = x

}
class TestX {

  class C[T](x: T) {
    def get: T = x
    def cmp: T => Boolean = (x == _)
  }

  val x: ([Y] =>> C[C[Y]])[_] = new C(new C("a")) // error: unreducible

  type CC[X] = C[C[X]]
  val y: CC[_] = ??? // error: unreducible

  type D[X] <: C[X]

  type DD = [X] =>> D[D[X]]
  val z: DD[_] = ??? // error: unreducible

  val g = x.get

  val c = x.cmp
}

object Test6014 {
  case class CC[T](key: T)
  type Alias[T] = Seq[CC[T]]

  def f(xs: Seq[CC[_]]) = xs map { case CC(x) => CC(x) }    // ok
  def g(xs: Alias[_])   = xs map { case CC(x) => CC(x) }    // error: unreducible application
}

