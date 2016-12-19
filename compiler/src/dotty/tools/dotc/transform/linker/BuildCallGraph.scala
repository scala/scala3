package dotty.tools.dotc.transform.linker

import dotty.tools.backend.jvm.CollectEntryPoints
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.transform.linker.callgraph.{CallGraph, CallGraphBuilder, GraphVisualization}
import dotty.tools.dotc.transform.linker.summaries.CallInfo

object BuildCallGraph {
  def isPhaseRequired(implicit ctx: Context): Boolean =
    DeadCodeElimination.isPhaseRequired || CallGraphChecks.isPhaseRequired || ctx.settings.linkVis.value

  def listPhase(implicit ctx: Context): List[Phase] = {
    if (isPhaseRequired) List(new BuildCallGraph)
    else Nil
  }
}

class BuildCallGraph extends Phase {

  import CallGraphBuilder._

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

    if (ctx.settings.YlinkDCEChecks.value) {
      for {
        methodSummaries <- collectedSummaries.valuesIterator
        callInfos <- methodSummaries.methodsCalled.valuesIterator
        callInfo <- callInfos
      } {
        CallInfo.check(callInfo)
      }
    }

    val callGraphBuilder = new CallGraphBuilder(collectedSummaries, mode, specLimit)

    var entryPointId = 0
    for (x <- collectedSummaries.valuesIterator) {
      if (isEntryPoint(x.methodDef)) {
        entryPointId += 1
        callGraphBuilder.pushEntryPoint(x.methodDef, entryPointId)
        x.methodDef.owner.ownersIterator.foreach { owner =>
          if (owner.is(Module)) {
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

//  private def sendSpecializationRequests(callGraph: CallGraph)(implicit ctx: Context): Unit = {
//   ctx.outerSpecPhase match {
//      case specPhase: OuterSpecializer =>
//
//        callGraph.reachableMethods.foreach { mc =>
//          val methodSym = mc.call.termSymbol
//          val outerTargs = methodSym.info.widen match {
//            case PolyType(names) =>
//              (names zip mc.targs).foldLeft(mc.outerTargs)((x, nameType) => x.+(methodSym, nameType._1, nameType._2))
//            case _ =>
//              mc.outerTargs
//          }
//          if (outerTargs.mp.nonEmpty && !methodSym.isPrimaryConstructor)
//            specPhase.registerSpecializationRequest(methodSym)(outerTargs)
//        }
//        callGraph.reachableTypes.foreach { tpc =>
//          val parentOverrides = tpc.tp.typeMembers(ctx).foldLeft(OuterTargs.empty)((outerTargs, denot) =>
//            denot.symbol.allOverriddenSymbols.foldLeft(outerTargs)((outerTargs, sym) =>
//              outerTargs.+(sym.owner, denot.symbol.name, denot.info)))
//
//          val spec = tpc.outerTargs ++ parentOverrides ++ parentRefinements(tpc.tp)
//
//          if (spec.nonEmpty) {
//            specPhase.registerSpecializationRequest(tpc.tp.typeSymbol)(spec)
//            def loop(remaining: List[Symbol]): Unit = {
//              if (remaining.isEmpty) return;
//              val target = remaining.head
//
//              val nspec = OuterTargs(spec.mp.filter{x => target.derivesFrom(x._1)})
//              if (nspec.nonEmpty)
//                specPhase.registerSpecializationRequest(target)(nspec)
//              loop(remaining.tail)
//            }
//            val parents = tpc.tp.baseClasses
//            loop(parents)
//          }
//        }
//      case _ =>
//       ctx.warning("No specializer phase found")
//    }
//  }

  private var runOnce = true
  def run(implicit ctx: Context): Unit = {
    if (runOnce && BuildCallGraph.isPhaseRequired) {
      val mode = AnalyseArgs
      val specLimit = 15

      ctx.log(s"\n\t\t\tType & Arg flow analisys")

      val callGraph = buildCallGraph(mode, specLimit)
      this.callGraph = callGraph

//      sendSpecializationRequests(callGraph)

      callGraph.getInfo().log()

      if (ctx.settings.linkVis.value) {
        val visFile = new java.io.File(ctx.settings.d.value + "-call-graph.html")
        GraphVisualization.outputGraphVisToFile(callGraph, visFile)
        ctx.log("Created call graph visualization: " + visFile.getAbsoluteFile)
      }

    }
    runOnce = false
  }

  private def isEntryPoint(s: Symbol)(implicit ctx: Context): Boolean = {
    ((s.name eq nme.main) /* for speed */  && s.is(Method) && CollectEntryPoints.isJavaMainMethod(s)) || // Java main method
      (s.is(Method) && s.hasAnnotation(defn.EntryPointAnnot)) // Explicit entry point
  }
}
