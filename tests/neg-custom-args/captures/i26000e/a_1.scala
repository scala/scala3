package i26000e

import language.experimental.captureChecking

class File
class Cell[A](@caps.unsafe.untrackedCaptures var elem: A)

// Capture sets nested under `^` in inferred types are not seen by other units
object Lib:
  val keep: Cell[File] = Cell(File())
  val flag: Boolean = false
  def g1[C^]: (k: (() -> File^{C})^) -> Unit = k => ()
  def g2[C^]: (k: (() -> File)^) -> Unit = k => keep.elem = k()
  def pick[C^] = if flag then g1[C] else g2[C]
