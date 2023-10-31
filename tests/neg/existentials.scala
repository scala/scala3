object TestList {

  var x: ([X] =>> List[List[X]])[?] = List(List(1)) // error: unreducible
  var y: ([X] =>> List[Seq[X]])[?] = List(List(1)) // error: unreducible

  x = x
  y = y
  y = x

  val h = x.head
  val x1: List[?] = h

  var z: List[?] = x

}
object TestSet {

  var x: ([Y] =>> Set[Set[Y]])[?] = Set(Set("a")) // error: unreducible
  var y: ([Y] =>> Set[Iterable[Y]])[?] = Set(Set("a")) // error: unreducible

  x = x
  y = y

  val h = x.head
  val h1: Set[?] = h

  // val p = x.+ // infinite loop in implicit search

  var z: Set[?] = x

}
class TestX {

  class C[T](x: T) {
    def get: T = x
    def cmp: T => Boolean = (x == _)
  }

  val x: ([Y] =>> C[C[Y]])[?] = new C(new C("a")) // error: unreducible

  type CC[X] = C[C[X]]
  val y: CC[?] = ??? // error: unreducible

  type D[X] <: C[X]

  type DD = [X] =>> D[D[X]]
  val z: DD[?] = ??? // error: unreducible

  val g = x.get

  val c = x.cmp
}

object Test6014 {
  case class CC[T](key: T)
  type Alias[T] = Seq[CC[T]]

  def f(xs: Seq[CC[?]]) = xs map { case CC(x) => CC(x) }    // ok
  def g(xs: Alias[?])   = xs map { case CC(x) => CC(x) }    // error: unreducible application
}

