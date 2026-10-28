package i26000inferredvar

import language.experimental.captureChecking
import scala.annotation.publicInBinary

class File
class Cell[A](@caps.unsafe.untrackedCaptures var value: A)
object io extends File, caps.SharedCapability

object Lib:
  val ioSink: Cell[File^{io}] = Cell(File())
  def m(x: File^{io}): Unit = ioSink.value = x
  def same[A](a: A): A = a

// Other units can assign a function that accepts only pure files to a var, or override
// a member with one. So the parameter type of the inferred type is the one they see, and
// passing io needs an explicit type.
class P uses io:
  @caps.unsafe.untrackedCaptures var f = Lib.same(Lib.m)
  def fire(): Unit = f(io) // error
  val g = Lib.same(Lib.m)
  def fireG(): Unit = g(io) // error
  def h() = Lib.same(Lib.m)
  def fireH(): Unit = h()(io) // error

// Subclasses in other units see the type of a protected[C] member, and inline methods
// expose a @publicInBinary member, with the capture sets in their inferred types dropped.
class Base uses io:
  protected[Base] val pc = Cell[(x: File^{io}) -> Unit](Lib.m) // error
  @publicInBinary private[Base] val pb = Cell[(x: File^{io}) -> Unit](Lib.m) // error
  inline def pbRef = pb
