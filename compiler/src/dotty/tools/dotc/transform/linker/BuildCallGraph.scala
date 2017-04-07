package dotty.tools.dotc.transform.linker

import dotty.tools.backend.jvm.CollectEntryPoints
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types.PolyType
import dotty.tools.dotc.transform.OuterSpecializer
import dotty.tools.dotc.transform.linker.callgraph._

object BuildCallGraph {
  def withJavaCallGraph(implicit ctx: Context): Boolean =
    ctx.settings.linkJavaConservative.value

  def withOutEdges(implicit ctx: Context): Boolean =
    ctx.settings.linkVis.value

  def isPhaseRequired(implicit ctx: Context): Boolean = {
    DeadCodeElimination.isPhaseRequired || CallGraphChecks.isPhaseRequired ||
    OuterSpecializer.isPhaseRequired || ctx.settings.linkVis.value
  }

  def listPhase(implicit ctx: Context): List[Phase] = {
    if (isPhaseRequired) List(new BuildCallGraph)
    else Nil
  }
}

class BuildCallGraph extends Phase {
  import BuildCallGraph._

  private var callGraph: CallGraph = _

  def phaseName: String = "callGraph"

  def getCallGraph: CallGraph = callGraph

  /**
    * @param mode see modes above
    * @param specLimit how many specializations symbol can have max
    * @return (reachableMethods, reachableTypes, casts, outerMethod)
    */
  private def buildCallGraph(mode: Int, specLimit: Int)(implicit ctx: Context): CallGraph = {
    val startTime = java.lang.System.currentTimeMillis()

    val collectedSummaries = ctx.summariesPhase.asInstanceOf[CollectSummaries].methodSummaries

    val callGraphBuilder = new CallGraphBuilder(collectedSummaries, mode, specLimit, withJavaCallGraph, withOutEdges)

    lazy val scalaApp = ctx.requiredClass("scala.App".toTypeName)
    lazy val scalaUtilPropertiesTrait = ctx.requiredClass("scala.util.PropertiesTrait".toTypeName)
    def isEntryPoint(s: Symbol): Boolean = {
      def filteredMain = (s.owner eq scalaApp) || (s.owner eq scalaUtilPropertiesTrait)
      ((s.name eq nme.main) /* for speed */  && s.is(Method) && CollectEntryPoints.isJavaMainMethod(s) && !filteredMain) || // Java main method
        (s.is(Method) && s.hasAnnotation(defn.EntryPointAnnot)) // Explicit entry point
    }

    var entryPointId = 0
    for (x <- collectedSummaries.valuesIterator) {
      if (isEntryPoint(x.methodDef)) {
        entryPointId += 1
        callGraphBuilder.pushEntryPoint(x.methodDef, entryPointId)
        x.methodDef.owner.ownersIterator.foreach { owner =>
          if (owner.is(Module) && !owner.isEmptyPackage) {
            val sourceModule = owner.sourceModule
            val moduleEntryPoint =
              if (sourceModule.owner.exists && !sourceModule.owner.isEmptyPackage) sourceModule
              else if (owner.primaryConstructor.exists) owner.primaryConstructor // workaround for modules in the empty package
              else NoSymbol
            if (moduleEntryPoint.exists)
              callGraphBuilder.pushEntryPoint(moduleEntryPoint, entryPointId)
          }
        }
      }
    }

    callGraphBuilder.build()

    val endTime = java.lang.System.currentTimeMillis()
    ctx.log("++++++++++ finished in " + (endTime - startTime)/1000.0  +" seconds. ++++++++++ ")

    callGraphBuilder.result()
  }

  private var runOnce = true
  def run(implicit ctx: Context): Unit = {
    if (runOnce && BuildCallGraph.isPhaseRequired) {
      val mode = CallGraphBuilder.AnalyseArgs
      val specLimit = 15

      ctx.log(s"\n\t\t\tType & Arg flow analysis")

      val callGraph = buildCallGraph(mode, specLimit)
      this.callGraph = callGraph

      if (OuterSpecializer.isPhaseRequired) {
        /*
        val outerSpecializer = ctx.phaseOfClass(classOf[OuterSpecializer]).asInstanceOf[OuterSpecializer]
        outerSpecializer.specializationRequest(callGraph)
        */
      }

      callGraph.getInfo().log()

      if (ctx.settings.linkVis.value) {
        val visFile = new java.io.File(ctx.settings.d.value + "/call-graph.html")
        GraphVisualization.outputGraphVisToFile(callGraph, visFile)
        ctx.log("Created call graph visualization: " + visFile.getAbsoluteFile)
      }

    }
    runOnce = false
  }
}
