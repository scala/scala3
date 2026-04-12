package dotty.tools.backend.jvm

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.report
import dotty.tools.dotc.core.*
import dotty.tools.dotc.interfaces.CompilerCallback
import Contexts.*
import Symbols.*
import dotty.tools.backend.ScalaPrimitives
import dotty.tools.backend.jvm.opt.{BCodeRepository, BTypesFromClassfile}
import dotty.tools.io.*

import scala.collection.mutable
import scala.compiletime.uninitialized
import java.util.concurrent.TimeoutException
import scala.concurrent.duration.Duration
import scala.concurrent.Await

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
 * which is why we have a "post-processor frontend interface" that hides this Context.
 */
class GenBCode extends Phase { self =>

  override def phaseName: String = GenBCode.name

  override def description: String = GenBCode.description

  override def isRunnable(using Context): Boolean = super.isRunnable && !ctx.usedBestEffortTasty

  private val entryPoints = new mutable.HashSet[String]()
  def registerEntryPoint(s: String): Unit = entryPoints += s


  private var _frontendAccess: PostProcessorFrontendAccess | Null = null
  def frontendAccess(using Context): PostProcessorFrontendAccess = {
    if _frontendAccess eq null then
      // Enforce usage of FreshContext so we would be able to modify compilation unit between runs
      val context = ctx match
        case fc: FreshContext => fc
        case ctx => ctx.fresh
      _frontendAccess = PostProcessorFrontendAccess.Impl(entryPoints)(context)
    _frontendAccess.nn
  }

  private var _primitives: ScalaPrimitives | Null = null
  def primitives(using Context): ScalaPrimitives = {
    if _primitives eq null then
      _primitives = ScalaPrimitives(ctx)
    _primitives.nn
  }

  private var _byteCodeRepository: BCodeRepository | Null = null
  def byteCodeRepository(using Context): BCodeRepository = {
    if _byteCodeRepository eq null then
      _byteCodeRepository = BCodeRepository(frontendAccess, backendUtils, bTypes)
    _byteCodeRepository.nn
  }

  private var _bTypesFromClassfile: BTypesFromClassfile | Null = null
  def bTypesFromClassfile(using Context): BTypesFromClassfile = {
    if _bTypesFromClassfile eq null then
      _bTypesFromClassfile = BTypesFromClassfile(byteCodeRepository, bTypes)
    _bTypesFromClassfile.nn
  }

  private var _bTypes: CoreBTypes | Null = null
  def bTypes(using Context): CoreBTypes = {
    if _bTypes eq null then
      // lazy load to break the circular dependency
      def inlineInfoLoader() = Option.when[InlineInfoLoader](frontendAccess.compilerSettings.optInlinerEnabled)(bTypesFromClassfile)
      _bTypes = CoreBTypesFromSymbols(frontendAccess, primitives, inlineInfoLoader)(using ctx)
    _bTypes.nn
  }

  private var _backendUtils: BackendUtils | Null = null
  def backendUtils(using Context): BackendUtils = {
    if _backendUtils eq null then
      _backendUtils = BackendUtils(frontendAccess, bTypes)
    _backendUtils.nn
  }

  private var _postProcessor: PostProcessor | Null = null
  def postProcessor(using Context): PostProcessor = {
    if _postProcessor eq null then
      _postProcessor = new PostProcessor(frontendAccess, byteCodeRepository, bTypesFromClassfile, backendUtils, bTypes)
    _postProcessor.nn
  }

  private var _codeGen: CodeGen | Null = null
  def codeGen(using Context): CodeGen = {
    if _codeGen eq null then
      _codeGen = new CodeGen(backendUtils, primitives, frontendAccess, postProcessor.callGraph, bTypes)
    _codeGen.nn
  }

  private var _generatedClassHandler: GeneratedClassHandler | Null = null
  def generatedClassHandler(using Context): GeneratedClassHandler = {
    if _generatedClassHandler eq null then
      _generatedClassHandler = GeneratedClassHandler(postProcessor)
    _generatedClassHandler.nn
  }

  protected def run(using Context): Unit =
    frontendAccess.frontendSynch {
      frontendAccess
      .ctx
      .setCompilationUnit(ctx.compilationUnit)
    }
    codeGen.genUnit(ctx.compilationUnit)
    (ctx.compilerCallback: CompilerCallback | Null) match {
      case cb: CompilerCallback => cb.onSourceCompiled(ctx.source)
      case null => ()
    }

  override def runOn(units: List[CompilationUnit])(using ctx:Context): List[CompilationUnit] = {
    try
      val result = super.runOn(units)
      generatedClassHandler.complete()
      result
    finally
      // frontendAccess and postProcessor are created lazily, clean them up only if they were initialized
      if _frontendAccess ne null then
        frontendAccess.compilerSettings.outputDirectory match {
          case jar: JarArchive =>
            if (ctx.run.nn.suspendedUnits.nonEmpty)
              // If we close the jar the next run will not be able to write on the jar.
              // But if we do not close it we cannot use it as part of the macro classpath of the suspended files.
              report.error("Can not suspend and output to a jar at the same time. See suspension with -Xprint-suspension.")

            jar.close()
          case _ => ()
        }
      if _postProcessor ne null then
        postProcessor.classfileWriter.close()
      generatedClassHandler.close()
  }
}

object GenBCode {
  val name: String = "genBCode"
  val description: String = "generate JVM bytecode"

  val CLASS_CONSTRUCTOR_NAME = "<clinit>"
  val INSTANCE_CONSTRUCTOR_NAME = "<init>"

}
