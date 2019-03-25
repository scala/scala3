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
import reporting.ThrowingReporter
import collection.mutable

object Pickler {
  val name: String = "pickler"
}

/** This phase pickles trees */
class Pickler extends Phase {
  import ast.tpd._

  override def phaseName: String = Pickler.name

  // No need to repickle trees comming from TASTY
  override def isRunnable(implicit ctx: Context): Boolean =
    super.isRunnable && !ctx.settings.fromTasty.value

  private def output(name: String, msg: String) = {
    val s = new PrintStream(name)
    s.print(msg)
    s.close
  }

  // Maps that keep a record if -Ytest-pickler is set.
  private val beforePickling = new mutable.HashMap[ClassSymbol, String]
  private val picklers = new mutable.HashMap[ClassSymbol, TastyPickler]

  /** Drop any elements of this list that are linked module classes of other elements in the list */
  private def dropCompanionModuleClasses(clss: List[ClassSymbol])(implicit ctx: Context): List[ClassSymbol] = {
    val companionModuleClasses =
      clss.filterNot(_ is Module).map(_.linkedClass).filterNot(_.unforcedIsAbsent)
    clss.filterNot(companionModuleClasses.contains)
  }

  override def run(implicit ctx: Context): Unit = {
    val unit = ctx.compilationUnit
    pickling.println(i"unpickling in run ${ctx.runId}")

    for { cls <- dropCompanionModuleClasses(topLevelClasses(unit.tpdTree))
          tree <- sliceTopLevel(unit.tpdTree, cls) } {
      val pickler = new TastyPickler(cls)
      if (ctx.settings.YtestPickler.value) {
        beforePickling(cls) = tree.show
        picklers(cls) = pickler
      }
      val treePkl = pickler.treePkl
      treePkl.pickle(tree :: Nil)
      treePkl.compactify()
      pickler.addrOfTree = treePkl.buf.addrOfTree
      pickler.addrOfSym = treePkl.addrOfSym
      if (tree.span.exists)
        new PositionPickler(pickler, treePkl.buf.addrOfTree).picklePositions(tree :: Nil)

      if (!ctx.settings.YdropComments.value)
        new CommentPickler(pickler, treePkl.buf.addrOfTree).pickleComment(tree)

      // other pickle sections go here.
      val pickled = pickler.assembleParts()
      unit.pickled += (cls -> pickled)

      def rawBytes = // not needed right now, but useful to print raw format.
        pickled.iterator.grouped(10).toList.zipWithIndex.map {
          case (row, i) => s"${i}0: ${row.mkString(" ")}"
        }
      // println(i"rawBytes = \n$rawBytes%\n%") // DEBUG
      if (pickling ne noPrinter) {
        println(i"**** pickled info of $cls")
        println(new TastyPrinter(pickled).printContents())
      }
    }
  }

  override def runOn(units: List[CompilationUnit])(implicit ctx: Context): List[CompilationUnit] = {
    val result = super.runOn(units)
    if (ctx.settings.YtestPickler.value)
      testUnpickler(
          ctx.fresh
            .setPeriod(Period(ctx.runId + 1, FirstPhaseId))
            .setReporter(new ThrowingReporter(ctx.reporter))
            .addMode(Mode.ReadPositions)
            .addMode(Mode.ReadComments)
            .addMode(Mode.PrintShowExceptions))
    result
  }

  private def testUnpickler(implicit ctx: Context): Unit = {
    pickling.println(i"testing unpickler at run ${ctx.runId}")
    ctx.initialize()
    val unpicklers =
      for ((cls, pickler) <- picklers) yield {
        val unpickler = new DottyUnpickler(pickler.assembleParts())
        unpickler.enter(roots = Set.empty)
        cls -> unpickler
      }
    pickling.println("************* entered toplevel ***********")
    for ((cls, unpickler) <- unpicklers) {
      val unpickled = unpickler.rootTrees
      testSame(i"$unpickled%\n%", beforePickling(cls), cls)
    }
  }

  private def testSame(unpickled: String, previous: String, cls: ClassSymbol)(implicit ctx: Context) =
    if (previous != unpickled) {
      output("before-pickling.txt", previous)
      output("after-pickling.txt", unpickled)
      ctx.error(s"""pickling difference for $cls in ${cls.source}, for details:
                   |
                   |  diff before-pickling.txt after-pickling.txt""".stripMargin)
    }
}
