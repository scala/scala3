package i26000runs

import language.experimental.captureChecking
import caps.any

class File
class Cell[A](@caps.unsafe.untrackedCaptures var value: A)
object io extends File, caps.SharedCapability

object Lib:
  def flag = true
  val ioSink: Cell[File^{io}] = Cell(io)
  val upE: Cell[[C^ <: {io}] => (x: File^{C}) -> Unit] = Cell([C^ <: {io}] => (x: File^{C}) => ())

// A later use extends a provisionally solved capture set in the inferred type of a local
// definition. Each capture checking run starts with new capture sets, so the use extends
// the set again in the next run. After the last run, this is an error.
object Use uses io:
  def merged(q: File^): File^{io} =
    val up = if Lib.flag then Cell([C^ <: {io}] => (x: File^{C}) => Lib.ioSink.value = x) else Cell([C^ <: {any.rd}] => (x: File^{C}) => ())
    up.value[{q}](q) // error
    Lib.ioSink.value
  def alias(r: File^): Unit =
    val c = Lib.upE
    c.value[{r}](r) // error
