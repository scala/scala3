package dotty.tools.dotc
package transform

import core._
import Contexts.Context
import Decorators._
import tasty._
import config.Printers.{noPrinter, pickling}
import java.io.PrintStream
import Periods._
import Phases._
import Symbols._
import Flags.Module
import collection.mutable
import util.Positions.Position

/** This phase pickles trees */
class Pickler extends Phase {
  import ast.tpd._

  override def phaseName: String = "pickler"

  private def output(name: String, msg: String) = {
    val s = new PrintStream(name)
    s.print(msg)
    s.close
  }

  private val beforePickling = new mutable.HashMap[ClassSymbol, (String, Position)]

  /** Drop any elements of this list that are linked module classes of other elements in the list */
  private def dropCompanionModuleClasses(clss: List[ClassSymbol])(implicit ctx: Context): List[ClassSymbol] = {
    val companionModuleClasses =
      clss.filterNot(_ is Module).map(_.linkedClass).filterNot(_.isAbsent)
    clss.filterNot(companionModuleClasses.contains)
  }

  override def run(implicit ctx: Context): Unit = {
    val unit = ctx.compilationUnit
    pickling.println(i"unpickling in run ${ctx.runId}")

    for { cls <- dropCompanionModuleClasses(topLevelClasses(unit.tpdTree))
          tree <- sliceTopLevel(unit.tpdTree, cls) } {
      if (ctx.settings.YtestPickler.value) beforePickling(cls) = (tree.show, tree.pos)
      val pickler = new TastyPickler()
      unit.picklers += (cls -> pickler)
      val treePkl = pickler.treePkl
      treePkl.pickle(tree :: Nil)
      treePkl.compactify()
      pickler.addrOfTree = treePkl.buf.addrOfTree
      pickler.addrOfSym = treePkl.addrOfSym
      if (tree.pos.exists)
        new PositionPickler(pickler, treePkl.buf.addrOfTree).picklePositions(tree :: Nil)

      def rawBytes = // not needed right now, but useful to print raw format.
        pickler.assembleParts().iterator.grouped(10).toList.zipWithIndex.map {
          case (row, i) => s"${i}0: ${row.mkString(" ")}"
        }
      // println(i"rawBytes = \n$rawBytes%\n%") // DEBUG
      if (pickling ne noPrinter) {
        println(i"**** pickled info of $cls")
        new TastyPrinter(pickler.assembleParts()).printContents()
      }
    }
  }

  override def runOn(units: List[CompilationUnit])(implicit ctx: Context): List[CompilationUnit] = {
    val result = super.runOn(units)
    if (ctx.settings.YtestPickler.value)
      testUnpickler(units)(ctx.fresh.setPeriod(Period(ctx.runId + 1, FirstPhaseId)))
    result
  }

  private def testUnpickler(units: List[CompilationUnit])(implicit ctx: Context): Unit = {
    pickling.println(i"testing unpickler at run ${ctx.runId}")
    ctx.initialize()
    val unpicklers: List[(ClassSymbol, DottyUnpickler)] =
      for (unit <- units; (cls, pickler) <- unit.picklers) yield {
        val unpickler = new DottyUnpickler(pickler.assembleParts())
        unpickler.enter(roots = Set())
        cls -> unpickler
      }
    pickling.println("*********** Entered toplevel ***********")
    for ((cls, unpickler) <- unpicklers) {
      val ((unpickled: Tree) :: Nil) = unpickler.body(ctx.addMode(Mode.ReadPositions))
      val (beforeShow, beforePos) = beforePickling(cls)
      testSameShow(unpickled.show, beforeShow, cls)
      testSamePos(unpickled.pos, beforePos, cls)
    }
  }

  private def testSameShow(unpickled: String, previous: String, cls: ClassSymbol)(implicit ctx: Context): Unit =
    if (previous != unpickled) {
      output("before-pickling.txt", previous)
      output("after-pickling.txt", unpickled)
      ctx.error(i"""pickling difference for ${cls.fullName} in ${cls.sourceFile}, for details:
                   |
                   |  diff before-pickling.txt after-pickling.txt""")
    }

  // Ignoring Position#point which are not pickled.
  private def testSamePos(unpickled: Position, previous: Position, cls: ClassSymbol)(implicit ctx: Context): Unit =
    if (unpickled.start != previous.start || unpickled.end != previous.end) {
      ctx.error(i"""pickling difference in ${cls.fullName} in ${cls.sourceFile} positions:
                   |previous positions $previous unpickled as $unpickled""")
    }
}
