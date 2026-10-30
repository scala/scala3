package i26000invariant

import language.experimental.captureChecking
import scala.annotation.publicInBinary

class File
class Cell[A](@caps.unsafe.untrackedCaptures var value: A)
object io extends File, caps.SharedCapability

object Lib:
  val ioSink: Cell[File^{io}] = Cell(File())
  def m(x: File^{io}): Unit = ioSink.value = x
  def withFile[T](op: (f: File^) => T): T = op(File())

// The parameter type of the eta-expansion of `m` is inferred. Other compilation units
// see these cells as Cell[(x: File) -> Unit] and can store functions in them that
// accept only pure files, so this unit must not pass io to their contents either.
class Base uses io:
  protected[Base] val pc = Cell(Lib.m) // subclasses in other units see it
  def fire(): Unit = pc.value(io) // error

object Impl uses io:
  private object impl:
    val c = Cell(Lib.m)
  inline def cellRef = impl.c // inlined in other units
  def fire(): Unit = impl.c.value(io) // error

object Binary uses io:
  @publicInBinary private[Binary] val pc = Cell(Lib.m)
  inline def cellRef = pc
  def fire(): Unit = pc.value(io) // error

// A `^` in a declared parameter type under an invariant type argument is a root of
// the definition, as in the same explicit type and as other compilation units see it.
// Capabilities of other definitions cannot flow into it.
object Roots:
  val ci = Cell((x: File^) => ())
  def read(p: File^): Unit = ci.value(p) // error
  def scoped(): Unit = Lib.withFile(f => ci.value(f)) // error
