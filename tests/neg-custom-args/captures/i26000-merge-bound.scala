package i26000mergebound

import language.experimental.captureChecking
import caps.any

class File
class Cell[A](@caps.unsafe.untrackedCaptures var value: A)
object io extends File, caps.SharedCapability

// The upper bounds {io} and {any.rd} cannot be merged, so the bound is inferred. Other
// units see the inferred bound as empty, so the use below cannot widen it: `up` needs an
// explicit type.
object Lib:
  def flag = true
  val up = if flag then Cell([C^ <: {io}] => (x: File^{C}) => ()) else Cell([C^ <: {any.rd}] => (x: File^{C}) => ())

object Use:
  def call(q: File^): Unit = Lib.up.value[{q}](q) // error
