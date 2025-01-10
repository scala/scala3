import scala.language.experimental.namedTuples

opaque type System = (wires: Any)

extension (system: System)
  inline def foo = system.wires
end extension

val simulation: System = ???
val _ = simulation.foo // was error
