opaque type NT[N <: Tuple, V <: Tuple] = V
opaque type System = NT[Tuple1["wires"], Tuple1[Any]]

extension [N <: Tuple, V <: Tuple] (x: NT[N, V]) {
  inline def apply(n: Int): Any =
    x.productElement(n)
}

extension (system: System) {
  inline def foo =
    system.apply(0)
}

val simulation: System = ???
val _ = simulation.foo
