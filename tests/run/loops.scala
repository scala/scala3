import scala.util.boundary, boundary.break

object loop:

  // We could use a boolean instead, but with `Ctrl` label types in using clauses are
  // more specific.
  enum Ctrl:
    case Exit, Continue

  inline def apply(inline op: boundary.Label[Ctrl] ?=> Unit) =
    while boundary { op; Ctrl.Continue } == Ctrl.Continue do ()

  inline def exit()(using boundary.Label[Ctrl]): Unit =
    break(Ctrl.Exit)

  inline def continue()(using boundary.Label[Ctrl]): Unit =
    break(Ctrl.Continue)
end loop

def testLoop(xs: List[Int]) =
  var current = xs
  var sum = 0
  loop:
    if current.isEmpty then loop.exit()
    val hd = current.head
    current = current.tail
    if hd == 0 then loop.exit()
    if hd < 0 then loop.continue()
    sum += hd
  sum

@main def Test =
  assert(testLoop(List(1, 2, 3, -2, 4, -3, 0, 1, 2, 3)) == 10)
  assert(testLoop(List()) == 0)
  assert(testLoop(List(-2, -3, 0, 1)) == 0)


