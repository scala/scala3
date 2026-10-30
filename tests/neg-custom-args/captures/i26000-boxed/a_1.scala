package i26000boxed

import language.experimental.captureChecking

class File
class Box[A](val value: A)
object io extends File, caps.SharedCapability

// Capture sets in upper bounds and in type arguments in declared parts are boxed.
// Other compilation units see these types as written.
object Lib:
  val up = Box([T <: File^{io}] => (x: T) => ())
  val dep = Box([C^, T <: File^{C}] => (x: T) => ())
  val xs = Box((xs: List[File^{io}]) => xs.length)
  val ub = Box([T <: List[File^{io}]] => (x: T) => 1)
  def mk(f: File^) = Box([T <: File^{f}] => (x: T) => ())
