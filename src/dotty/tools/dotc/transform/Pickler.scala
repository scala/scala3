package dotty.tools.dotc
package transform

import core._
import TreeTransforms._
import Contexts.Context
import Decorators._
import pickling._

/** This miniphase pickles trees */
class Pickler extends MiniPhaseTransform { thisTransform =>
  import ast.tpd._

  override def phaseName: String = "pickler"
  
  
  override def transformUnit(tree: Tree)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val pickler = new TastyPickler
    new TreePickler(pickler, picklePositions = false).pickle(tree)
    val bytes = pickler.assembleParts()
    def rawBytes = 
      bytes.iterator.grouped(10).toList.zipWithIndex.map { 
        case (row, i) => s"${i}0: ${row.mkString(" ")}"
      }
    //println(s"written:\n${rawBytes.mkString("\n")}")
    new TastyPrinter(bytes).printContents()
    tree
  }
}