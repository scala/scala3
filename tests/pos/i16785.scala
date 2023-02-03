class VarImpl[Lbl, A]

class Outer[|*|[_, _], Lbl1]:
  type Var[A1] = VarImpl[Lbl1, A1]

  sealed trait Foo[G]
  case class Bar[T, U]()
    extends Foo[Var[T] |*| Var[U]]

  def go[X](scr: Foo[Var[X]]): Unit = scr match // was: compile hang
    case Bar() => ()
