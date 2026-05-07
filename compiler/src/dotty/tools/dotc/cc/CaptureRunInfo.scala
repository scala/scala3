package dotty.tools.dotc
package cc

import core.Contexts.{Context, ctx}
import core.Names.TermName
import core.Types.Type
import config.Printers.capt
import util.EqHashMap

trait CaptureRunInfo:
  self: Run =>
  private var maxSize = 0
  private var maxPath: List[CaptureSet.DerivedVar] = Nil

  /** Backing table for [[ClosureParamNames]] — see that file for the
   *  read/write protocol. Don't access this field directly; use the
   *  `ClosureParamNames` API.
   */
  private[cc] val closureParamNames: EqHashMap[Type, List[TermName]] = EqHashMap()

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
    closureParamNames.clear()
end CaptureRunInfo
