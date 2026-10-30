package i26000runs

import language.experimental.captureChecking

class File
class Cell[A](@caps.unsafe.untrackedCaptures var value: A)
object io extends File, caps.SharedCapability

object Lib:
  val ioSink: Cell[File^{io}] = Cell(io)
  val upE: Cell[[C^ <: {io}] => (x: File^{C}) -> Unit] = Cell([C^ <: {io}] => (x: File^{C}) => ())

// A later use cannot extend a provisionally solved capture set in the inferred type of a
// local definition, since another capture checking run would create the set anew.
object Use uses io:
  def alias(r: File^): File^{io} =
    val c = Lib.upE
    c.value[{r}](r) // error
    Lib.ioSink.value
