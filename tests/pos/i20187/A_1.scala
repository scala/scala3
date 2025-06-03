import scala.deriving.Mirror

enum E:
  case Foo1()
  case Foo2()

class Outer:
  case class Inner()
val o = new Outer

type F = E.Foo1
type G = Tuple.Head[E.Foo1 *: E.Foo2 *: EmptyTuple]
type H = Tuple.Head[o.Inner *: EmptyTuple]
type I = Tuple.Last[E *: EmptyTuple]

def local =
  case class Bar()
  type B = Tuple.Head[Bar *: EmptyTuple]
  summon[Mirror.Of[B]]
