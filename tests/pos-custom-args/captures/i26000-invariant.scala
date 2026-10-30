package i26000invariant

import language.experimental.captureChecking

class File
class Box[A](val value: A)
class Cell[A](@caps.unsafe.untrackedCaptures var value: A)
object io extends File, caps.SharedCapability

object Lib:
  val sink: Cell[File] = Cell(File())
  val ioSink: Cell[File^{io}] = Cell(File())
  def m(x: File^{io}): Unit = ioSink.value = x
  val pureSink: (x: File) -> Unit = x => sink.value = x
  def takesPure(c: Cell[(x: File) -> Unit]): Unit = c.value = pureSink
  def takesPureBox(b: Box[(x: File) -> Unit]): Unit = ()

// The parameter type of the eta-expansion of `m` is inferred. Under an invariant
// type argument it is closed at its lower bound, in local definitions as well.
object Local uses io:
  def passPure(): Unit =
    val c = Cell(Lib.m)
    Lib.takesPure(c)
  def retPure(): Cell[(x: File) -> Unit] =
    val c = Cell(Lib.m)
    c
  def passPureBox(): Unit =
    val b = Box(Lib.m)
    Lib.takesPureBox(b)
  def writePure(): Unit =
    val c = Cell(Lib.m)
    c.value = Lib.pureSink

// A `^` in a declared parameter type under an invariant type argument is a root of
// the definition, as in the same explicit type
object Roots uses io:
  val ci = Cell((x: File^) => ())
  val ce: Cell[(x: File^) -> Unit] = Cell((x: File^) => ())
  def readIo(): Unit = ci.value(io)
  def copy(): Unit =
    ci.value = ce.value
    ce.value = ci.value
