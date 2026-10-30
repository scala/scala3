package i26000c

class File
class Cell[A](@caps.unsafe.untrackedCaptures var elem: A)
object io extends File, caps.SharedCapability

// Other compilation units see the parameter type as written
object Lib uses io:
  val cell = Cell((x: File^{io}) => ())
  def run(): Unit = cell.elem(io)
