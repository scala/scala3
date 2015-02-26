package dotty.tools.dotc
package transform

import core._
import TreeTransforms._
import Contexts.Context
import Decorators._
import pickling._
import config.Printers

/** This miniphase pickles trees */
class Pickler extends MiniPhaseTransform { thisTransform =>
  import ast.tpd._

  override def phaseName: String = "pickler"
  
  override def transformUnit(tree: Tree)(implicit ctx: Context, info: TransformerInfo): Tree = {
    if (!ctx.compilationUnit.isJava) {
      val pickler = new TastyPickler
      
      val treePkl = new TreePickler(pickler)
      treePkl.pickle(tree :: Nil)
      new PositionPickler(pickler, treePkl.buf.addrOfTree).picklePositions(tree :: Nil, tree.pos)

      val bytes = pickler.assembleParts()
      ctx.compilationUnit.pickled = bytes
      def rawBytes = // not needed right now, but useful to print raw format.
        bytes.iterator.grouped(10).toList.zipWithIndex.map {
          case (row, i) => s"${i}0: ${row.mkString(" ")}"
        }
      // println(i"rawBytes = \n$rawBytes%\n%") // DEBUG
      if (Printers.pickling ne Printers.noPrinter) new TastyPrinter(bytes).printContents()
      
      //println(i"unpickled:\n ${new DottyUnpickler(bytes, readPositions = false).result}%\n%")
      
    }
    tree
  }
}