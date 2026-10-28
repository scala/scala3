package i26000invariantbound

import language.experimental.captureChecking

class File
class Cell[A](@caps.unsafe.untrackedCaptures var value: A)
object io extends File, caps.SharedCapability

// A local in a declared upper bound under an invariant type argument cannot be dropped,
// so the bound is inferred. Other units see it as empty, so the use below cannot widen
// it: `up` needs an explicit type.
object Lib:
  val up = { val loc: File^ = File(); Cell([C^ <: {io, loc}] => (x: File^{C}) => ()) }

object Use:
  def call(q: File^): Unit = Lib.up.value[{q}](q) // error
