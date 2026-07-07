package dotty.tools.backend.jvm

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.report
import dotty.tools.dotc.core.*
import dotty.tools.dotc.interfaces.CompilerCallback
import Contexts.*
import dotty.tools.backend.ScalaPrimitives
import dotty.tools.backend.jvm.opt.{BCodeRepository, BTypesFromClassfile, CallGraph, OptimizerKnownBTypes, OptimizerUtils}
import dotty.tools.dotc.core.Decorators.em
import dotty.tools.io.*

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


  private var _optimizerUtils: OptimizerUtils | Null = null
  def optimizerUtils(using Context): OptimizerUtils = {
    if _optimizerUtils eq null then
      _optimizerUtils = OptimizerUtils(optimizerKnownBTypes)
    _optimizerUtils.nn
  }

  private var _primitives: ScalaPrimitives | Null = null
  def primitives(using Context): ScalaPrimitives = {
    if _primitives eq null then
      _primitives = ScalaPrimitives()
    _primitives.nn
  }

  private var _byteCodeRepository: BCodeRepository | Null = null
  def byteCodeRepository(using Context): BCodeRepository = {
    if _byteCodeRepository eq null then
      _byteCodeRepository = BCodeRepository(ctx.platform.classPath, optimizerUtils)
    _byteCodeRepository.nn
  }

  private var _bTypesFromClassfile: BTypesFromClassfile | Null = null
  def bTypesFromClassfile(using Context): BTypesFromClassfile = {
    if _bTypesFromClassfile eq null then
      _bTypesFromClassfile = BTypesFromClassfile(byteCodeRepository, bTypeLoader)
    _bTypesFromClassfile.nn
  }

  private var _bTypeLoader: BTypeLoader | Null = null
  def bTypeLoader(using Context): BTypeLoader = {
    if _bTypeLoader eq null then
      // lazy load to break the circular dependency
      def inlineInfoLoader() = Option.when[InlineInfoLoader](ctx.settings.optInlineEnabled)(bTypesFromClassfile)
      _bTypeLoader = BTypeLoader(primitives, inlineInfoLoader)
    _bTypeLoader.nn
  }

  private var _knownBTypes: KnownBTypes | Null = null
  def knownBTypes(using Context): KnownBTypes = {
    if _knownBTypes eq null then
      if ctx.settings.optInlineEnabled || ctx.settings.optClosureInvocations then
        _knownBTypes = optimizerKnownBTypes // avoid creating two separate instances of this
      else
        _knownBTypes = KnownBTypes(bTypeLoader)(using ctx)
    _knownBTypes.nn
  }

  private var _optimizerKnownBTypes: OptimizerKnownBTypes | Null = null
  def optimizerKnownBTypes(using Context): OptimizerKnownBTypes = {
    if _optimizerKnownBTypes eq null then
      _optimizerKnownBTypes = OptimizerKnownBTypes(bTypeLoader)(using ctx)
    _optimizerKnownBTypes.nn
  }

  private var _callGraph: CallGraph | Null = null
  def callGraph(using Context): CallGraph = {
    if _callGraph eq null then
      _callGraph = new CallGraph(byteCodeRepository, bTypesFromClassfile)
    _callGraph.nn
  }

  private var _postProcessor: PostProcessor | Null = null
  def postProcessor(using Context): PostProcessor = {
    if _postProcessor eq null then
      if ctx.settings.optInlineEnabled || ctx.settings.optClosureInvocations then
        _postProcessor = new PostProcessorWithOptimizations(byteCodeRepository, bTypesFromClassfile, callGraph, optimizerUtils, bTypeLoader, optimizerKnownBTypes)
      else
        _postProcessor = new PostProcessor(bTypeLoader, knownBTypes)
    _postProcessor.nn
  }

  private var _generatedClassHandler: GeneratedClassHandler | Null = null
  def generatedClassHandler(using Context): GeneratedClassHandler = {
    if _generatedClassHandler eq null then {
      val handler = ctx.settings.YbackendParallelism.value match {
        case 1 => GeneratedClassHandler.serial(postProcessor)
        case maxThreads =>
          // The thread pool queue is limited in size. When it's full, the `CallerRunsPolicy` causes
          // a new task to be executed on the main thread, which provides back-pressure.
          // The queue size is large enough to ensure that running a task on the main thread does
          // not take longer than to exhaust the queue for the backend workers.
          val queueSize = ctx.settings.YbackendWorkerQueue.valueSetByUser.getOrElse(maxThreads * 2)
          GeneratedClassHandler.parallel(postProcessor, maxThreads, queueSize, this, ctx.profiler)
      }
      _generatedClassHandler =
        if ctx.settings.optInlineEnabled || ctx.settings.optClosureInvocations
        then GeneratedClassHandler.withGlobalOptimizations(handler)
        else handler
    }
    _generatedClassHandler.nn
  }

  private var _codeGen: CodeGen | Null = null
  def codeGen(using Context): CodeGen = {
    if _codeGen eq null then
      val cg = Option.when(ctx.settings.optInlineEnabled || ctx.settings.optClosureInvocations)(callGraph)
      _codeGen = new CodeGen(primitives, cg, bTypeLoader, knownBTypes, generatedClassHandler)
    _codeGen.nn
  }

  protected def run(using Context): Unit =
    codeGen.genUnit()
    ctx.compilerCallback match
      case cb: CompilerCallback => cb.onSourceCompiled(ctx.source)
      case null => ()

  override def runOn(units: List[CompilationUnit])(using ctx:Context): List[CompilationUnit] = {
    // as long as we might initialize `generatedClassHandler` (or anything else) in here, we must set the context's phase,
    // otherwise we'll initialize stuff like KnownBTypes with a context at the wrong phase, thus the symbols won't have the denotations we expect
    given unitCtx: Context = ctx.fresh.setPhase(this)
    try
      val result = super.runOn(units)
      for (exn, f) <- generatedClassHandler.complete() do
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
      // created lazily, clean them up only if they were initialized
      if _postProcessor ne null then
        postProcessor.close()
      if _generatedClassHandler ne null then
        generatedClassHandler.close()
  }
}

object GenBCode {
  val name: String = "genBCode"
  val description: String = "generate JVM bytecode"
}
