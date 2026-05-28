package dotty.tools.backend.jvm

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.report
import dotty.tools.dotc.core.*
import dotty.tools.dotc.interfaces.CompilerCallback
import Contexts.*
import dotty.tools.backend.ScalaPrimitives
import dotty.tools.backend.jvm.opt.{BCodeRepository, BTypesFromClassfile, CallGraph}
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


  private var _backendUtils: BackendUtils | Null = null
  def backendUtils(using Context): BackendUtils = {
    if _backendUtils eq null then
      _backendUtils = BackendUtils(wellKnownBTypes)
    _backendUtils.nn
  }

  private var _frontendAccess: PostProcessorFrontendAccess | Null = null
  def frontendAccess(using Context): PostProcessorFrontendAccess = {
    if _frontendAccess eq null then
      _frontendAccess = PostProcessorFrontendAccess(ctx)
    _frontendAccess.nn
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
      _byteCodeRepository = BCodeRepository(ctx.platform.classPath, backendUtils)
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

  private var _wellKnownBTypes: WellKnownBTypes | Null = null
  def wellKnownBTypes(using Context): WellKnownBTypes = {
    if _wellKnownBTypes eq null then
      // lazy load to break the circular dependency
      def inlineInfoLoader() = Option.when[InlineInfoLoader](ctx.settings.optInlineEnabled)(bTypesFromClassfile)
      _wellKnownBTypes = WellKnownBTypes(frontendAccess, bTypeLoader)(using ctx)
    _wellKnownBTypes.nn
  }

  private var _callGraph: CallGraph | Null = null
  def callGraph(using Context): CallGraph = {
    if _callGraph eq null then
      _callGraph = new CallGraph(frontendAccess, byteCodeRepository, bTypesFromClassfile)
    _callGraph.nn
  }

  private var _postProcessor: PostProcessor | Null = null
  def postProcessor(using Context): PostProcessor = {
    if _postProcessor eq null then
      _postProcessor = new PostProcessor(frontendAccess, byteCodeRepository, bTypesFromClassfile, callGraph, backendUtils, bTypeLoader, wellKnownBTypes)
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
      _codeGen = new CodeGen(backendUtils, primitives, frontendAccess, callGraph, bTypeLoader, wellKnownBTypes, generatedClassHandler)
    _codeGen.nn
  }

  protected def run(using Context): Unit =
    codeGen.genUnit()
    ctx.compilerCallback match
      case cb: CompilerCallback => cb.onSourceCompiled(ctx.source)
      case null => ()

  override def runOn(units: List[CompilationUnit])(using ctx:Context): List[CompilationUnit] = {
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

  val CLASS_CONSTRUCTOR_NAME = "<clinit>"
  val INSTANCE_CONSTRUCTOR_NAME = "<init>"

}
