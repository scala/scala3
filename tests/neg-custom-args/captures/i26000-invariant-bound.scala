package i26000invariantbound

import language.experimental.captureChecking

class File
class Cell[A](@caps.unsafe.untrackedCaptures var value: A)
object io extends File, caps.SharedCapability

// A local in a declared upper bound under an invariant type argument is dropped from
// the bound, so uses in this unit cannot widen the bound beyond what other units see.
// This file has no other errors, so that a widening would not be hidden by a skipped
// capture checking run.
object Lib:
  val up = { val loc: File^ = File(); Cell([C^ <: {io, loc}] => (x: File^{C}) => ()) }

object Use:
  def call(q: File^): Unit = Lib.up.value[{q}](q) // error
