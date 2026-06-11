package dotty.tools.backend.jvm

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.report
import dotty.tools.dotc.core.*
import dotty.tools.dotc.interfaces.CompilerCallback
import Contexts.*
import dotty.tools.backend.ScalaPrimitives
import dotty.tools.backend.jvm.opt.{BCodeRepository, BTypesFromClassfile, OptimizerCallGraph, IndyLambdaImplTracker, OptimizerKnownBTypes}
import dotty.tools.dotc.core.Decorators.em
import dotty.tools.io.*

import scala.annotation.stableNull
import scala.collection.mutable
import scala.compiletime.uninitialized

/**
 * GenBCode has 3 parts:
 * 1. Translating trees to Java bytecode
 * 2. Optimizing the bytecode, if the user requested it
 * 3. Emitting the bytecode to class files.
 *
 * Part 1 requires a Context, and must therefore be done sequentially.
 * Parts 2 and 3 do not require a Context and can be parallelized.
 *
 * It is crucial that parts 2 and 3 do not accidentally depend on a Context,
 * which is why we have abstractions to hide it such as OptimizerSettings.
 */
class GenBCode extends Phase { self =>
  override def phaseName: String = GenBCode.name
  override def description: String = GenBCode.description
  override def isRunnable(using Context): Boolean = super.isRunnable && !ctx.usedBestEffortTasty

  private var _initialized: Boolean = false
  private var _codeGen: CodeGen = uninitialized
  private var _postProcessor: PostProcessor = uninitialized
  private var _generatedClassHandler: GeneratedClassHandler = uninitialized

  private def ensureInit()(using Context): Unit =
    if _initialized then
      return
    def createClassHandler(postProcessor: PostProcessor) = ctx.settings.YbackendParallelism.value match {
      case 1 => GeneratedClassHandler.serial(postProcessor)
      case maxThreads =>
        // The thread pool queue is limited in size. When it's full, the `CallerRunsPolicy` causes
        // a new task to be executed on the main thread, which provides back-pressure.
        // The queue size is large enough to ensure that running a task on the main thread does
        // not take longer than to exhaust the queue for the backend workers.
        val queueSize = ctx.settings.YbackendWorkerQueue.valueSetByUser.getOrElse(maxThreads * 2)
        GeneratedClassHandler.parallel(postProcessor, maxThreads, queueSize, this, ctx.profiler)
    }
    val primitives = new ScalaPrimitives()
    val classBTypeCache = new ClassBType.Cache()
    if ctx.settings.optInlineEnabled || ctx.settings.optClosureInvocations then
      val indyTracker = new IndyLambdaImplTracker()
      val byteCodeRepository = new BCodeRepository(ctx.platform.classPath, indyTracker)
      val bTypesFromClassfile = new BTypesFromClassfile(byteCodeRepository, classBTypeCache)
      val bTypeLoader = new BTypeLoader(primitives, classBTypeCache, Some(bTypesFromClassfile))
      val knownBTypes = new OptimizerKnownBTypes(bTypeLoader)
      val callGraph = new OptimizerCallGraph(byteCodeRepository, bTypesFromClassfile)
      _postProcessor = new PostProcessorWithOptimizations(classBTypeCache, byteCodeRepository, bTypesFromClassfile, callGraph, indyTracker, knownBTypes)
      _generatedClassHandler = GeneratedClassHandler.withGlobalOptimizations(createClassHandler(_postProcessor))
      object impl extends BCodeIdiomatic(callGraph), BCodeHelpers(bTypeLoader), BCodeBodyBuilder(primitives, knownBTypes), BCodeSyncAndTry
      _codeGen = new CodeGen(impl)
    else
      val bTypeLoader = new BTypeLoader(primitives, classBTypeCache, None)
      val knownBTypes = new KnownBTypes(bTypeLoader)
      _postProcessor = new PostProcessor(classBTypeCache, knownBTypes)
      _generatedClassHandler = createClassHandler(_postProcessor)
      object impl extends BCodeIdiomatic(DisabledCallGraph), BCodeHelpers(bTypeLoader), BCodeBodyBuilder(primitives, knownBTypes), BCodeSyncAndTry
      _codeGen = new CodeGen(impl)
    _initialized = true

  protected def run(using Context): Unit =
    ensureInit()
    _generatedClassHandler.process(_codeGen.genUnit())
    ctx.compilerCallback match
      case cb: CompilerCallback => cb.onSourceCompiled(ctx.source)
      case null => ()

  override def runOn(units: List[CompilationUnit])(using Context): List[CompilationUnit] = {
    try
      val result = super.runOn(units)
      if _initialized then
        for (exn, f) <- _generatedClassHandler.complete() do
          report.error(em"unable to write $f $exn")
          exn.printStackTrace()
      result
    finally
      ctx.settings.outputDir.value match
        case jar: JarArchive =>
          if (ctx.run.nn.suspendedUnits.nonEmpty)
            // If we close the jar the next run will not be able to write on the jar.
            // But if we do not close it we cannot use it as part of the macro classpath of the suspended files.
            report.error("Cannot suspend and output to a jar at the same time. See suspension with -Xprint-suspension.")
          jar.close()
        case _ => ()
      if _initialized then
        _postProcessor.close()
        _generatedClassHandler.close()
  }
}

object GenBCode {
  val name: String = "genBCode"
  val description: String = "generate JVM bytecode"
}
