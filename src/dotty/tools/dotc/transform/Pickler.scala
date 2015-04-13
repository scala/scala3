package dotty.tools.dotc
package transform

import core._
import Contexts.Context
import Decorators._
import pickling._
import config.Printers.{noPrinter, pickling}
import java.io.PrintStream
import Periods._
import Phases._
import collection.mutable

/** This phase pickles trees */
class Pickler extends Phase {
  import ast.tpd._

  override def phaseName: String = "pickler"

  private def output(name: String, msg: String) = {
    val s = new PrintStream(name)
    s.print(msg)
    s.close
  }

  private val beforePickling = new mutable.HashMap[CompilationUnit, String]

  override def run(implicit ctx: Context): Unit = {
    val unit = ctx.compilationUnit
    val tree = unit.tpdTree
    pickling.println(i"unpickling in run ${ctx.runId}")
    if (ctx.settings.YtestPickler.value) beforePickling(unit) = tree.show

    val pickler = unit.pickler
    val treePkl = new TreePickler(pickler)
    treePkl.pickle(tree :: Nil)
    unit.addrOfTree = treePkl.buf.addrOfTree
    unit.addrOfSym = treePkl.addrOfSym
    if (tree.pos.exists)
      new PositionPickler(pickler, treePkl.buf.addrOfTree).picklePositions(tree :: Nil, tree.pos)

    def rawBytes = // not needed right now, but useful to print raw format.
      unit.pickler.assembleParts().iterator.grouped(10).toList.zipWithIndex.map {
        case (row, i) => s"${i}0: ${row.mkString(" ")}"
      }
    // println(i"rawBytes = \n$rawBytes%\n%") // DEBUG
    if (pickling ne noPrinter) new TastyPrinter(pickler.assembleParts()).printContents()
  }

  override def runOn(units: List[CompilationUnit])(implicit ctx: Context): List[CompilationUnit] = {
    val result = super.runOn(units)
    if (ctx.settings.YtestPickler.value)
      testUnpickler(units)(ctx.fresh.setPeriod(Period(ctx.runId + 1, FirstPhaseId)))
    result
  }

  private def testUnpickler(units: List[CompilationUnit])(implicit ctx: Context): Unit = {
    pickling.println(i"testing unpickler at run ${ctx.runId}")
    ctx.definitions.init
    val unpicklers =
      for (unit <- units) yield {
        val unpickler = new DottyUnpickler(unit.pickler.assembleParts())
        unpickler.enter(roots = Set())
        unpickler
      }
    pickling.println("************* entered toplevel ***********")
    for ((unpickler, unit) <- unpicklers zip units) {
      val unpickled = unpickler.body(readPositions = false)
      testSame(i"$unpickled%\n%", beforePickling(unit), unit)
    }
  }

  private def testSame(unpickled: String, previous: String, unit: CompilationUnit)(implicit ctx: Context) =
    if (previous != unpickled) {
      output("before-pickling.txt", previous)
      output("after-pickling.txt", unpickled)
      ctx.error(s"""pickling difference for $unit, for details:
                   |
                   |  diff before-pickling.txt after-pickling.txt""".stripMargin)
    }
}
