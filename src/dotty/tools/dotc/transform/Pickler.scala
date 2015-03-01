package dotty.tools.dotc
package transform

import core._
import TreeTransforms._
import Contexts.Context
import Decorators._
import pickling._
import config.Printers
import java.io.PrintStream
import Periods._

/** This miniphase pickles trees */
class Pickler extends MiniPhaseTransform { thisTransform =>
  import ast.tpd._

  override def phaseName: String = "pickler"
  
  private def output(name: String, msg: String) = {
    val s = new PrintStream(name)
    s.print(msg)
    s.close
  }
  
  override def transformUnit(tree: Tree)(implicit ctx: Context, info: TransformerInfo): Tree = {
    if (!ctx.compilationUnit.isJava) {
      val pickler = new TastyPickler
      println(i"unpickling in run ${ctx.runId}")
      val previous = if (ctx.settings.YtestPickler.value) tree.show else ""
                  
      val treePkl = new TreePickler(pickler)
      treePkl.pickle(tree :: Nil)
      if (tree.pos.exists)
        new PositionPickler(pickler, treePkl.buf.addrOfTree).picklePositions(tree :: Nil, tree.pos)

      val bytes = pickler.assembleParts()
      ctx.compilationUnit.pickled = bytes
      def rawBytes = // not needed right now, but useful to print raw format.
        bytes.iterator.grouped(10).toList.zipWithIndex.map {
          case (row, i) => s"${i}0: ${row.mkString(" ")}"
        }
      // println(i"rawBytes = \n$rawBytes%\n%") // DEBUG
      if (Printers.pickling ne Printers.noPrinter) new TastyPrinter(bytes).printContents()
      
      if (ctx.settings.YtestPickler.value)
        unpickle(bytes, previous)(ctx.fresh.setPeriod(Period(ctx.runId + 1, FirstPhaseId)))
    }
    tree 
  }

  private def unpickle(bytes: Array[Byte], previous: String)(implicit ctx: Context) = {
    println(i"unpickle run = ${ctx.runId}")
    ctx.definitions.init
    val unpickled = i"${new DottyUnpickler(bytes, readPositions = false).result}%\n%"
    println(i"previous :\n $previous")
    println(i"unpickled:\n $unpickled")
    if (previous != unpickled) {
      output("before-pickling.txt", previous)
      output("after-pickling.txt", unpickled)
      println(s"""pickling difference for ${ctx.compilationUnit}, for details:
                 |
                 |  diff before-pickling.txt after-pickling.txt""".stripMargin)
    }
  }
}