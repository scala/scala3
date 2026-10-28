package i26000copiedlocals

import language.experimental.captureChecking

class File
object io extends File, caps.SharedCapability

// Declared parameter types that mention locals of a right hand side are copied into the
// inferred types of other definitions, which can be pickled with another top-level class.
object Lib:
  val w = { val loc: File^ = File(); (x: File^{loc}) => () }

def mk() = { val loc: File^ = File(); (x: File^{io, loc}) => () }

object B:
  val l = List(Lib.w)
  def n(): Int = List(mk()).length
