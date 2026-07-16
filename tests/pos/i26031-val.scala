trait Mirror:
  type Elems

trait Generic[T]:
  type Repr

class Box[M]

object Generic:
  transparent inline def apply[T](using m: Mirror) =
    new Generic[T] { type Repr = Box[m.type] }

class Foo
def mkMirror(): Mirror = ???

object Test:
  val g = Generic[Foo](using mkMirror())
