package dotty.tools
package dotc
package transform

import core.*
import Contexts.*
import Decorators.*
import tasty.*
import config.Printers.{noPrinter, pickling}
import config.Feature
import java.io.PrintStream
import io.FileWriters.{TastyWriter, ReadOnlyContext}
import StdNames.{str, nme}
import Periods.*
import Phases.*
import Symbols.*
import Flags.Module
import reporting.{ThrowingReporter, Profile, Message}
import collection.mutable
import util.concurrent.{Executor, Future}
import compiletime.uninitialized
import dotty.tools.io.{JarArchive, AbstractFile}
import dotty.tools.dotc.printing.OutlinePrinter
import scala.annotation.constructorOnly
import scala.concurrent.Promise
import dotty.tools.dotc.transform.Pickler.writeSigFilesAsync

import scala.util.chaining.given
import dotty.tools.io.FileWriters.BufferingDelayedReporting

object Pickler {
  val name: String = "pickler"
  val description: String = "generates TASTy info"

  /** If set, perform jump target compacting, position and comment pickling,
   *  as well as final assembly in parallel with downstream phases; force
   *  only in backend.
   */
  inline val ParallelPickling = true

  class AsyncTastyHolder(val earlyOut: AbstractFile, val promise: Promise[AsyncTastyState])
  class AsyncTastyState(val hasErrors: Boolean, val pending: Option[BufferingDelayedReporting])

  // Why we only write to early output in the first run?
  // ===================================================
  // TL;DR the point of pipeline compilation is to start downstream projects early,
  // so we don't want to wait for suspended units to be compiled.
  //
  // But why is it safe to ignore suspended units?
  // If this project contains a transparent macro that is called in the same project,
  // the compilation unit of that call will be suspended (if the macro implementation
  // is also in this project), causing a second run.
  // However before we do that run, we will have already requested sbt to begin
  // early downstream compilation. This means that the suspended definitions will not
  // be visible in *early* downstream compilation.
  //
  // However, sbt will by default prevent downstream compilation happening in this scenario,
  // due to the existence of macro definitions. So we are protected from failure if user tries
  // to use the suspended definitions.
  //
  // Additionally, it is recommended for the user to move macro implementations to another project
  // if they want to force early output. In this scenario the suspensions will no longer occur, so now
  // they will become visible in the early-output.
  //
  // See `sbt-test/pipelining/pipelining-scala-macro` and `sbt-test/pipelining/pipelining-scala-macro-force`
  // for examples of this in action.
  //
  // Therefore we only need to write to early output in the first run. We also provide the option
  // to diagnose suspensions with the `-Yno-suspended-units` flag.
  def writeSigFilesAsync(
      tasks: List[(String, Array[Byte])],
      writer: EarlyFileWriter,
      promise: Promise[AsyncTastyState])(using ctx: ReadOnlyContext): Unit = {
    try
      for (internalName, pickled) <- tasks do
        val _ = writer.writeTasty(internalName, pickled)
    finally
      try
        writer.close()
      finally
        promise.success(
          AsyncTastyState(
            hasErrors = ctx.reporter.hasErrors,
            pending = (
              ctx.reporter match
                case buffered: BufferingDelayedReporting => Some(buffered)
                case _ => None
            )
          )
        )
      end try
    end try
  }

  class EarlyFileWriter private (writer: TastyWriter, origin: AbstractFile):
    def this(dest: AbstractFile)(using @constructorOnly ctx: ReadOnlyContext) = this(TastyWriter(dest), dest)

    export writer.writeTasty

    def close(): Unit =
      writer.close()
      origin match {
        case jar: JarArchive => jar.close() // also close the file system
        case _ =>
      }
}

/** This phase pickles trees */
class Pickler extends Phase {
  import ast.tpd.*

  def doAsyncTasty(using Context): Boolean = ctx.asyncTastyPromise.isDefined

  override def phaseName: String = Pickler.name

  override def description: String = Pickler.description

  // No need to repickle trees coming from TASTY
  override def isRunnable(using Context): Boolean =
    super.isRunnable && (!ctx.settings.fromTasty.value || doAsyncTasty)

  // when `-Yjava-tasty` is set we actually want to run this phase on Java sources
  override def skipIfJava(using Context): Boolean = false

  private def output(name: String, msg: String) = {
    val s = new PrintStream(name)
    s.print(msg)
    s.close
  }

  // Maps that keep a record if -Ytest-pickler is set.
  private val beforePickling = new mutable.HashMap[ClassSymbol, String]
  private val printedTasty = new mutable.HashMap[ClassSymbol, String]
  private val pickledBytes = new mutable.HashMap[ClassSymbol, (CompilationUnit, Array[Byte])]

  /** Drop any elements of this list that are linked module classes of other elements in the list */
  private def dropCompanionModuleClasses(clss: List[ClassSymbol])(using Context): List[ClassSymbol] = {
    val companionModuleClasses =
      clss.filterNot(_.is(Module)).map(_.linkedClass).filterNot(_.isAbsent())
    clss.filterNot(companionModuleClasses.contains)
  }

  /** Runs given functions with a scratch data block in a serialized fashion (i.e.
   *  inside a synchronized block). Scratch data is re-used between calls.
   *  Used to conserve on memory usage by avoiding to create scratch data for each
   *  pickled unit.
   */
  object serialized:
    val scratch = new ScratchData
    private val buf = mutable.ListBuffer.empty[(String, Array[Byte])]
    def run(body: ScratchData => Array[Byte]): Array[Byte] =
      synchronized {
        scratch.reset()
        body(scratch)
      }
    def commit(internalName: String, tasty: Array[Byte]): Unit = synchronized {
      buf += ((internalName, tasty))
    }
    def result(): List[(String, Array[Byte])] = synchronized {
      val res = buf.toList
      buf.clear()
      res
    }

  private val executor = Executor[Array[Byte]]()

  private def useExecutor(using Context) = Pickler.ParallelPickling && !ctx.settings.YtestPickler.value

  private def printerContext(isOutline: Boolean)(using Context): Context =
    if isOutline then ctx.fresh.setPrinterFn(OutlinePrinter(_))
    else ctx

  /** only ran under -Ypickle-write and -from-tasty */
  private def runFromTasty(unit: CompilationUnit)(using Context): Unit = {
    val pickled = unit.pickled
    for (cls, bytes) <- pickled do
      serialized.commit(computeInternalName(cls), bytes())
  }

  private def computeInternalName(cls: ClassSymbol)(using Context): String =
    if cls.is(Module) then cls.binaryClassName.stripSuffix(str.MODULE_SUFFIX).nn
    else cls.binaryClassName

  override def run(using Context): Unit = {
    val unit = ctx.compilationUnit
    pickling.println(i"unpickling in run ${ctx.runId}")

    if ctx.settings.fromTasty.value then
      // skip the rest of the phase, as tasty is already "pickled",
      // however we still need to set up tasks to write TASTy to
      // early output when pipelining is enabled.
      if doAsyncTasty then
        runFromTasty(unit)
      return ()

    for
      cls <- dropCompanionModuleClasses(topLevelClasses(unit.tpdTree))
      tree <- sliceTopLevel(unit.tpdTree, cls)
    do
      if ctx.settings.YtestPickler.value then beforePickling(cls) =
        tree.show(using printerContext(unit.typedAsJava))

      val sourceRelativePath =
        val reference = ctx.settings.sourceroot.value
        util.SourceFile.relativePath(unit.source, reference)
      val isJavaAttr = unit.isJava // we must always set JAVAattr when pickling Java sources
      if isJavaAttr then
        // assert that Java sources didn't reach Pickler without `-Yjava-tasty`.
        assert(ctx.settings.YjavaTasty.value, "unexpected Java source file without -Yjava-tasty")
      val isOutline = isJavaAttr // TODO: later we may want outline for Scala sources too
      val attributes = Attributes(
        sourceFile = sourceRelativePath,
        scala2StandardLibrary = ctx.settings.YcompileScala2Library.value,
        explicitNulls = ctx.settings.YexplicitNulls.value,
        captureChecked = Feature.ccEnabled,
        withPureFuns = Feature.pureFunsEnabled,
        isJava = isJavaAttr,
        isOutline = isOutline
      )

      val pickler = new TastyPickler(cls)
      val treePkl = new TreePickler(pickler, attributes)
      treePkl.pickle(tree :: Nil)
      Profile.current.recordTasty(treePkl.buf.length)

      val positionWarnings = new mutable.ListBuffer[Message]()
      def reportPositionWarnings() = positionWarnings.foreach(report.warning(_))

      val internalName = if doAsyncTasty then computeInternalName(cls) else ""

      def computePickled(): Array[Byte] = inContext(ctx.fresh) {
        serialized.run { scratch =>
          treePkl.compactify(scratch)
          if tree.span.exists then
            val reference = ctx.settings.sourceroot.value
            PositionPickler.picklePositions(
                pickler, treePkl.buf.addrOfTree, treePkl.treeAnnots, reference,
                unit.source, tree :: Nil, positionWarnings,
                scratch.positionBuffer, scratch.pickledIndices)

          if !ctx.settings.YdropComments.value then
            CommentPickler.pickleComments(
                pickler, treePkl.buf.addrOfTree, treePkl.docString, tree,
                scratch.commentBuffer)

          AttributePickler.pickleAttributes(attributes, pickler, scratch.attributeBuffer)

          val pickled = pickler.assembleParts()

          def rawBytes = // not needed right now, but useful to print raw format.
            pickled.iterator.grouped(10).toList.zipWithIndex.map {
              case (row, i) => s"${i}0: ${row.mkString(" ")}"
            }

          // println(i"rawBytes = \n$rawBytes%\n%") // DEBUG
          if ctx.settings.YprintTasty.value || pickling != noPrinter then
            println(i"**** pickled info of $cls")
            println(TastyPrinter.showContents(pickled, ctx.settings.color.value == "never"))
            println(i"**** end of pickled info of $cls")

          if doAsyncTasty then
            serialized.commit(internalName, pickled)

          pickled
        }
      }

      /** A function that returns the pickled bytes. Depending on `Pickler.ParallelPickling`
       *  either computes the pickled data in a future or eagerly before constructing the
       *  function value.
       */
      val demandPickled: () => Array[Byte] =
        if useExecutor then
          val futurePickled = executor.schedule(computePickled)
          () =>
            try futurePickled.force.get
            finally reportPositionWarnings()
        else
          val pickled = computePickled()
          reportPositionWarnings()
          if ctx.settings.YtestPickler.value then
            pickledBytes(cls) = (unit, pickled)
            if ctx.settings.YtestPicklerCheck.value then
              printedTasty(cls) = TastyPrinter.showContents(pickled, noColor = true, testPickler = true)
          () => pickled

      unit.pickled += (cls -> demandPickled)
    end for
  }

  override def runOn(units: List[CompilationUnit])(using Context): List[CompilationUnit] = {
    val isConcurrent = useExecutor

    val writeTask: Option[() => Unit] = ctx.asyncTastyPromise.map: holder =>
      () =>
        given ReadOnlyContext = if isConcurrent then ReadOnlyContext.buffered else ReadOnlyContext.eager
        val writer = Pickler.EarlyFileWriter(holder.earlyOut)
        writeSigFilesAsync(serialized.result(), writer, holder.promise)

    def runPhase(writeCB: (doWrite: () => Unit) => Unit) =
      super.runOn(units).tap(_ => writeTask.foreach(writeCB))

    val result =
      if isConcurrent then
        executor.start()
        try
          runPhase: doWrite =>
            // unless we redesign executor to have "Unit" schedule overload, we need some sentinel value.
            executor.schedule(() => { doWrite(); Array.emptyByteArray })
        finally executor.close()
      else
        runPhase(_())
    if ctx.settings.YtestPickler.value then
      val ctx2 = ctx.fresh
        .setSetting(ctx.settings.YreadComments, true)
        .setSetting(ctx.settings.YshowPrintErrors, true)
      testUnpickler(
        using ctx2
          .setPeriod(Period(ctx.runId + 1, ctx.base.typerPhase.id))
          .setReporter(new ThrowingReporter(ctx.reporter))
          .addMode(Mode.ReadPositions)
      )
    result
  }

  private def testUnpickler(using Context): Unit =
    pickling.println(i"testing unpickler at run ${ctx.runId}")
    ctx.initialize()
    val resolveCheck = ctx.settings.YtestPicklerCheck.value
    val unpicklers =
      for ((cls, (unit, bytes)) <- pickledBytes) yield {
        val unpickler = new DottyUnpickler(unit.source.file, bytes)
        unpickler.enter(roots = Set.empty)
        val optCheck =
          if resolveCheck then
            val resolved = unit.source.file.resolveSibling(s"${cls.name.mangledString}.tastycheck")
            if resolved == null then None
            else Some(resolved)
          else None
        cls -> (unit, unpickler, optCheck)
      }
    pickling.println("************* entered toplevel ***********")
    val rootCtx = ctx
    for ((cls, (unit, unpickler, optCheck)) <- unpicklers) do
      val testJava = unit.typedAsJava
      if testJava then
        if unpickler.unpickler.nameAtRef.contents.exists(_ == nme.FromJavaObject) then
          report.error(em"Pickled reference to FromJavaObject in Java defined $cls in ${cls.source}")
      val unpickled = unpickler.rootTrees
      val freshUnit = CompilationUnit(rootCtx.compilationUnit.source)
      freshUnit.needsCaptureChecking = unit.needsCaptureChecking
      freshUnit.knowsPureFuns = unit.knowsPureFuns
      optCheck match
        case Some(check) =>
          import java.nio.charset.StandardCharsets.UTF_8
          val checkContents = String(check.toByteArray, UTF_8)
          inContext(rootCtx.fresh.setCompilationUnit(freshUnit)):
            testSamePrinted(printedTasty(cls), checkContents, cls, check)
        case None =>
          ()

      inContext(printerContext(testJava)(using rootCtx.fresh.setCompilationUnit(freshUnit))):
        testSame(i"$unpickled%\n%", beforePickling(cls), cls)

  private def testSame(unpickled: String, previous: String, cls: ClassSymbol)(using Context) =
    import java.nio.charset.StandardCharsets.UTF_8
    def normal(s: String) = new String(s.getBytes(UTF_8), UTF_8)
    val unequal = unpickled.length() != previous.length() || normal(unpickled) != normal(previous)
    if unequal then
      output("before-pickling.txt", previous)
      output("after-pickling.txt", unpickled)
      //sys.process.Process("diff -u before-pickling.txt after-pickling.txt").!
      report.error(em"""pickling difference for $cls in ${cls.source}, for details:
                    |
                    |  diff before-pickling.txt after-pickling.txt""")
  end testSame

  private def testSamePrinted(printed: String, checkContents: String, cls: ClassSymbol, check: AbstractFile)(using Context): Unit = {
    for lines <- diff(printed, checkContents) do
      output("after-printing.txt", printed)
      report.error(em"""TASTy printer difference for $cls in ${cls.source}, did not match ${check},
                    |  output dumped in after-printing.txt, check diff with `git diff --no-index -- $check after-printing.txt`
                    |  actual output:
                    |$lines%\n%""")
  }

  /** Reuse diff logic from compiler/test/dotty/tools/vulpix/FileDiff.scala */
  private def diff(actual: String, expect: String): Option[Seq[String]] =
    import scala.util.Using
    import scala.io.Source
    val actualLines = Using(Source.fromString(actual))(_.getLines().toList).get
    val expectLines = Using(Source.fromString(expect))(_.getLines().toList).get
    Option.when(!matches(actualLines, expectLines))(actualLines)

  private def matches(actual: String, expect: String): Boolean = {
    import java.io.File
    val actual1 = actual.stripLineEnd
    val expect1  = expect.stripLineEnd

    // handle check file path mismatch on windows
    actual1 == expect1 || File.separatorChar == '\\' && actual1.replace('\\', '/') == expect1
  }

  private def matches(actual: Seq[String], expect: Seq[String]): Boolean = {
    actual.length == expect.length
    && actual.lazyZip(expect).forall(matches)
  }
}
