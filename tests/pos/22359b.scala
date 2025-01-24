object Obj2:
  opaque type NT[N <: Tuple, V <: Tuple] = V

  extension [N <: Tuple, V <: Tuple] (x: NT[N, V]) {
    inline def apply(n: Int): Any =
      x.productElement(n)
  }

object Obj:
  opaque type System = Obj2.NT[Tuple1["wires"], Tuple1[Any]]

  extension (system: System) {
    inline def foo = system.apply(0)
  }
import Obj._
val simulation: System = ???
val _ = simulation.foo
