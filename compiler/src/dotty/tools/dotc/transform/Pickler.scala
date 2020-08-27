package dotty.tools.dotc
package transform

import core._
import Contexts._
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
import scala.concurrent.{Future, Await, ExecutionContext}
import scala.concurrent.duration.Duration

object Pickler {
  val name: String = "pickler"
}

/** This phase pickles trees */
class Pickler extends Phase {
  import ast.tpd._

  override def phaseName: String = Pickler.name

  // No need to repickle trees coming from TASTY
  override def isRunnable(using Context): Boolean =
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
  private def dropCompanionModuleClasses(clss: List[ClassSymbol])(using Context): List[ClassSymbol] = {
    val companionModuleClasses =
      clss.filterNot(_.is(Module)).map(_.linkedClass).filterNot(_.isAbsent())
    clss.filterNot(companionModuleClasses.contains)
  }

  override def run(using Context): Unit = {
    val unit = ctx.compilationUnit
    pickling.println(i"unpickling in run ${ctx.runId}")

    for
      cls <- dropCompanionModuleClasses(topLevelClasses(unit.tpdTree))
      tree <- sliceTopLevel(unit.tpdTree, cls)
    do
      val pickler = new TastyPickler(cls)
      if ctx.settings.YtestPickler.value then
        beforePickling(cls) = tree.show
        picklers(cls) = pickler
      val treePkl = pickler.treePkl
      treePkl.pickle(tree :: Nil)
      val positionWarnings = new mutable.ListBuffer[String]()
      val pickledF = inContext(ctx.fresh) {
        Future {
          treePkl.compactify()
          if tree.span.exists then
            new PositionPickler(pickler, treePkl.buf.addrOfTree, treePkl.treeAnnots)
              .picklePositions(tree :: Nil, positionWarnings)

          if !ctx.settings.YdropComments.value then
            new CommentPickler(pickler, treePkl.buf.addrOfTree, treePkl.docString)
              .pickleComment(tree)

          val pickled = pickler.assembleParts()

          def rawBytes = // not needed right now, but useful to print raw format.
            pickled.iterator.grouped(10).toList.zipWithIndex.map {
              case (row, i) => s"${i}0: ${row.mkString(" ")}"
            }

          // println(i"rawBytes = \n$rawBytes%\n%") // DEBUG
          if pickling ne noPrinter then
            pickling.synchronized {
              println(i"**** pickled info of $cls")
              println(new TastyPrinter(pickled).printContents())
            }
          pickled
        }(using ExecutionContext.global)
      }
      def force(): Array[Byte] =
        val result = Await.result(pickledF, Duration.Inf)
        positionWarnings.foreach(report.warning(_))
        result

      // Turn off parallelism because it lead to non-deterministic CI failures:
      // - https://github.com/lampepfl/dotty/runs/1029579877?check_suite_focus=true#step:10:967
      // - https://github.com/lampepfl/dotty/runs/1027582846?check_suite_focus=true#step:10:564
      //
      // Turning off parallelism in -Ytest-pickler and Interactive mode seems to
      // work around the problem but I would prefer to not enable this at all
      // until the root cause of the issue is understood since it might manifest
      // itself in other situations too.
      if !ctx.settings.YparallelPickler.value then force()

      unit.pickled += (cls -> force)
    end for
  }

  override def runOn(units: List[CompilationUnit])(using Context): List[CompilationUnit] = {
    val result = super.runOn(units)
    if ctx.settings.YtestPickler.value
      testUnpickler(
        using ctx.fresh
            .setPeriod(Period(ctx.runId + 1, FirstPhaseId))
            .setReporter(new ThrowingReporter(ctx.reporter))
            .addMode(Mode.ReadPositions)
            .addMode(Mode.ReadComments)
            .addMode(Mode.PrintShowExceptions))
    result
  }

  private def testUnpickler(using Context): Unit = {
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

  private def testSame(unpickled: String, previous: String, cls: ClassSymbol)(using Context) =
    if (previous != unpickled) {
      output("before-pickling.txt", previous)
      output("after-pickling.txt", unpickled)
      report.error(s"""pickling difference for $cls in ${cls.source}, for details:
                   |
                   |  diff before-pickling.txt after-pickling.txt""".stripMargin)
    }
}
