package dotty.tools.dotc.transform.linker.callgraph

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types._

object TypeDepth {

  def apply(tp: Type)(implicit ctx: Context): Int =
    max(tp.classSymbols.iterator.map(classDepth))

  private def classDepth(cls: ClassSymbol)(implicit ctx: Context): Int =
    1 + max(cls.classParents.iterator.map(apply))

  private def max(it: Iterator[Int]) = if (it.hasNext) it.max else 0
}
