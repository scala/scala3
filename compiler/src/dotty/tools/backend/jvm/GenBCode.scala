package dotty.tools.backend.jvm

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.report
import dotty.tools.dotc.core.*
import dotty.tools.dotc.interfaces.CompilerCallback
import Contexts.*
import dotty.tools.backend.ScalaPrimitives
import dotty.tools.backend.jvm.opt.{BCodeRepository, BTypesFromClassfile, CallGraph, IndyLambdaImplTracker, OptimizerKnownBTypes, OptimizerUtils}
import dotty.tools.dotc.core.Decorators.em
import dotty.tools.io.*

import scala.annotation.stableNull
import scala.collection.mutable

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

  @stableNull
  private var _codeGen: CodeGen | Null = null

  private def ensureInit()(using Context): CodeGen = _codeGen match
    case c: CodeGen => c
    case null =>
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
      _codeGen =
        if ctx.settings.optInlineEnabled || ctx.settings.optClosureInvocations then
          val indyTracker = new IndyLambdaImplTracker()
          val byteCodeRepository = new BCodeRepository(ctx.platform.classPath, indyTracker)
          val bTypesFromClassfile = new BTypesFromClassfile(byteCodeRepository, classBTypeCache)
          val bTypeLoader = new BTypeLoader(primitives, classBTypeCache, Some(bTypesFromClassfile))
          val knownBTypes = new OptimizerKnownBTypes(bTypeLoader)
          val callGraph = new CallGraph(byteCodeRepository, bTypesFromClassfile)
          val postProcessor = new PostProcessorWithOptimizations(classBTypeCache, byteCodeRepository, bTypesFromClassfile, callGraph, indyTracker, knownBTypes)
          val classHandler = GeneratedClassHandler.withGlobalOptimizations(createClassHandler(postProcessor))
          new CodeGen(primitives, Some(callGraph), bTypeLoader, knownBTypes, postProcessor, classHandler)
        else
          val bTypeLoader = new BTypeLoader(primitives, classBTypeCache, None)
          val knownBTypes = new KnownBTypes(bTypeLoader)
          val postProcessor = new PostProcessor(classBTypeCache, knownBTypes)
          val classHandler = createClassHandler(postProcessor)
          new CodeGen(primitives, None, bTypeLoader, knownBTypes, postProcessor, classHandler)
      _codeGen

  protected def run(using Context): Unit =
    ensureInit().genUnit()
    ctx.compilerCallback match
      case cb: CompilerCallback => cb.onSourceCompiled(ctx.source)
      case null => ()

  override def runOn(units: List[CompilationUnit])(using Context): List[CompilationUnit] = {
    try
      val result = super.runOn(units)
      if _codeGen ne null then
        for (exn, f) <- _codeGen.generatedClassHandler.complete() do
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
      if _codeGen ne null then
        _codeGen.postProcessor.close()
        _codeGen.generatedClassHandler.close()
  }
}

object GenBCode {
  val name: String = "genBCode"
  val description: String = "generate JVM bytecode"
}
