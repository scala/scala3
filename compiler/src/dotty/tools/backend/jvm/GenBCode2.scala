package dotty.tools.backend.jvm

import dotty.tools.backend.ScalaPrimitives
import dotty.tools.backend.jvm.opt.*
import dotty.tools.dotc.{CompilationUnit, report}
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.io.JarArchive

/**
 * Code generation has 3 parts:
 * 1. Translating trees to Java bytecode
 * 2. Optimizing the bytecode, if the user requested it
 * 3. Emitting the bytecode to class files.
 *
 * Part 1 requires a Context, and must therefore be done sequentially.
 * Parts 2 and 3 do not require a Context and can be parallelized.
 *
 * It is crucial that parts 2 and 3 do not accidentally depend on a Context,
 * which is why we have abstractions to eagerly fetch data from it such as OptimizerSettings.
 */
final class GenBCode2 extends Phase {
  override def phaseName: String = GenBCode.name
  override def description: String = GenBCode.description
  override def isRunnable(using ctx: Context): Boolean = super.isRunnable && !ctx.usedBestEffortTasty

  private var codeGen: CodeGen2 | Null = null
  private def getCodeGen()(using ctx: Context): CodeGen2 = codeGen match {
    case null =>
      val primitives = ScalaPrimitives()
      val classBTypeCache = new ClassBType.Cache()
      val gen =
        if ctx.settings.optInlineEnabled || ctx.settings.optClosureInvocations then
          val indyTracker = new IndyLambdaImplTracker()
          val byteCodeRepository = new BCodeRepository(ctx.platform.classPath, indyTracker)
          val bTypesFromClassfile = new BTypesFromClassfile(byteCodeRepository, classBTypeCache)
          val bTypeLoader = new BTypeLoader(primitives, classBTypeCache, Some(bTypesFromClassfile))
          val knownBTypes = new OptimizerKnownBTypes(bTypeLoader)
          val callGraph = new OptimizerCallGraph(byteCodeRepository, bTypesFromClassfile)
          val bc = BCode(knownBTypes, bTypeLoader, primitives, callGraph)
          val optSettings = new OptimizerSettings()
          val closureOptimizer = new ClosureOptimizer(indyTracker, byteCodeRepository, callGraph, knownBTypes, bTypesFromClassfile, optSettings)
          val heuristics = new InlinerHeuristics(byteCodeRepository, callGraph, knownBTypes, optSettings)
          val globalOpt = new GlobalOptimizer(indyTracker, callGraph, classBTypeCache, bTypesFromClassfile, byteCodeRepository, heuristics, closureOptimizer, optSettings)
          val localOpt = new LocalOptimizer(indyTracker, callGraph, globalOpt, knownBTypes, bTypesFromClassfile, optSettings)
          CodeGen2(this, bc, Some(localOpt), Some(globalOpt))
        else
          val bTypeLoader = new BTypeLoader(primitives, classBTypeCache, None)
          val knownBTypes = new KnownBTypes(bTypeLoader)
          val bc = BCode(knownBTypes, bTypeLoader, primitives, DisabledCallGraph)
          CodeGen2(this, bc, None, None)
      codeGen = gen
      gen
    case cg => cg
  }

  protected override def run(using ctx: Context): Unit =
    getCodeGen().addCompilationUnit()

  override def runOn(units: List[CompilationUnit])(using ctx: Context): List[CompilationUnit] = {
    try
      super.runOn(units)
    finally {
      ctx.settings.outputDir.value match
        case jar: JarArchive =>
          if (ctx.run.nn.suspendedUnits.nonEmpty)
            // If we close the jar the next run will not be able to write on the jar.
            // But if we do not close it we cannot use it as part of the macro classpath of the suspended files.
            report.error("Cannot suspend and output to a jar at the same time. See suspension with -Xprint-suspension.")
          jar.close()
        case _ => ()
      codeGen match
        case null => () // no compilation units, that's OK
        case cg => cg.finish()
    }
  }
}
