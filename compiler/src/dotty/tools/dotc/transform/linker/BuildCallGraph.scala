package dotty.tools.dotc.transform.linker

import dotty.tools.backend.jvm.CollectEntryPoints
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.transform.linker.callgraph.{CallGraph, CallGraphBuilder, GraphVisualization}

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

    val callGraphBuilder = new CallGraphBuilder(mode)

    var entryPointId = 0
    ctx.summariesPhase.asInstanceOf[CollectSummaries].methodSummaries.foreach { x =>
      if (isEntryPoint(x.methodDef)) {
        entryPointId += 1
        callGraphBuilder.pushEntryPoint(x.methodDef, entryPointId)
        x.methodDef.owner.ownersIterator.foreach { owner =>
          if (owner.is(Module) && !owner.isEmptyPackage) {
            val sourceModule = owner.sourceModule
            if (sourceModule.owner.exists && !sourceModule.owner.isEmptyPackage) // FIXME this condition should be removed
                callGraphBuilder.pushEntryPoint(sourceModule, entryPointId)
          }
        }
      }
    }

    callGraphBuilder.build()

    val endTime = java.lang.System.currentTimeMillis()
    println("++++++++++ finished in " + (endTime - startTime)/1000.0  +" seconds. ++++++++++ ")

    callGraphBuilder.result()
  }

  private def sendSpecializationRequests(callGraph: CallGraph)(implicit ctx: Context): Unit = {
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

  }

  private var runOnce = true
  def run(implicit ctx: Context): Unit = {
    if (runOnce /*&& ctx.settings.lto.value.nonEmpty*/) {
      val specLimit = 15
      //println(s"\n\t\t\tOriginal analisys")
      //val g1 = buildCallGraph(AnalyseOrig, specLimit)

      //println(s"\n\t\t\tType flow analisys")
      //val g2 = buildCallGraph(AnalyseTypes, specLimit)

      println(s"\n\t\t\tType & Arg flow analisys")
      val callGraph = buildCallGraph(AnalyseArgs, specLimit)
      this.callGraph = callGraph

//      println(GraphVisualization.outputDiagnostic(AnalyseArgs, specLimit)(callGraph))

      val viz = GraphVisualization.outputGraphVis(AnalyseArgs, specLimit)(callGraph)

      val g3 = GraphVisualization.outputGraph(AnalyseArgs, specLimit)(callGraph)

      sendSpecializationRequests(callGraph)

      def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit): Unit = {
        val p = new java.io.PrintWriter(f)
        try { op(p) } finally { p.close() }
      }

      //printToFile(new java.io.File("out1.dot")) { out =>
      //  out.println(g1)
      //}
     // printToFile(new java.io.File("out2.dot")) { out =>
     //   out.println(g2)
     // }
      printToFile(new java.io.File("CallGraph.dot")) { out =>
        out.println(g3)
      }
      printToFile(new java.io.File("callgraph.html")) { out =>
        out.println(viz)
      }

    }
    runOnce = false
  }

  private def isEntryPoint(s: Symbol)(implicit ctx: Context): Boolean = {
    ((s.name eq nme.main) /* for speed */  && s.is(Method) && CollectEntryPoints.isJavaMainMethod(s)) || // Java main method
      (s.is(Method) && s.hasAnnotation(defn.ExportAnnot)) // Explicit entry point
  }
}
