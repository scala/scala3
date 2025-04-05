package dotty.tools.dotc
package cc

import core.Contexts.{Context, ctx}
import config.Printers.capt

trait CaptureRunInfo:
  self: Run =>
  private var maxSize = 0
  private var maxPath: List[CaptureSet.DerivedVar] = Nil

  def recordPath(size: Int, path: => List[CaptureSet.DerivedVar]): Unit =
    if size > maxSize then
      maxSize = size
      maxPath = path

  def printMaxPath()(using Context): Unit =
    if maxSize > 0 then
      println(s"max derived capture set path length: $maxSize")
      println(s"max derived capture set path: ${maxPath.map(_.summarize).reverse}")

  protected def reset(): Unit =
    maxSize = 0
    maxPath = Nil
end CaptureRunInfo
