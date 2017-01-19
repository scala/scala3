package dotty.tools.dotc.transform.linker.callgraph

import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Types.{ConstantType, PolyType, Type}
import dotty.tools.dotc.transform.linker.types.ClosureType

case class CallGraph(entryPoints: Map[CallInfoWithContext, Int], reachableMethods: Set[CallInfoWithContext],
                     reachableTypes: Set[TypeWithContext], casts: Set[Cast], classOfs: Set[Symbol], outerMethods: Set[Symbol],
                     mode: Int, specLimit: Int)(implicit ctx: Context) { self =>

  private val reachableSet: Set[Symbol] =
    reachableMethods.map(_.callSymbol)

  private val reachableClassesSet: Set[Symbol] =
    reachableTypes.flatMap(x => x.tp.classSymbol :: x.tp.baseClasses)

  def isReachableMethod(sym: Symbol): Boolean = reachableSet.contains(sym)

  def isReachableClass(sym: Symbol): Boolean = reachableClassesSet.contains(sym)

  def isReachableClassOf(sym: Symbol): Boolean = classOfs.contains(sym)

  def getInfo(): Info = new Info

  class Info {

    lazy val classesWithReachableMethods = reachableSet.map(_.maybeOwner.info.widen.classSymbol)

    lazy val reachableClasses = classesWithReachableMethods ++ reachableClassesSet

    lazy val reachableDefs = reachableMethods.map(_.callSymbol)

    /*val filter = scala.io.Source.fromFile("trace-filtered").getLines().toList
    /val filterUnMangled = filter.map(x => x.replace("::", ".").replace("$class", "")).toSet

    def fil(x: Symbol) =
      filterUnMangled.contains(x.fullName.toString)

    val liveDefs = reachableDefs.filter{x => fil(x)}     */


    lazy val reachableSpecs: Set[(Symbol, List[Type])] = reachableMethods.flatMap { x =>
      val clas = x.callSymbol.maybeOwner.info.widen.classSymbol
      val meth = x.callSymbol
      if (mode >= CallGraphBuilder.AnalyseTypes) (meth, x.call.normalizedPrefix.baseArgInfos(clas)) :: Nil
      else {
        val clazSpecializationsCount =
          if (clas.primaryConstructor.info.widenDealias.isInstanceOf[PolyType]) specLimit
          else 1
        val methodSpecializationsCount =
          if (meth.info.widenDealias.isInstanceOf[PolyType]) specLimit
          else 1
        ctx.log(s"specializing $clas $meth for $clazSpecializationsCount * $methodSpecializationsCount")
        (0 until clazSpecializationsCount*methodSpecializationsCount).map(x => (meth, ConstantType(Constant(x)):: Nil)).toList
      }
    }

    private lazy val morphisms = reachableMethods.groupBy(x => x.callee).groupBy(x => x._2.map(_.callSymbol).toSet.size)

    lazy val monomorphicCalls = if (morphisms.contains(1)) morphisms(1) else Map.empty

    lazy val bimorphicCalls = if (morphisms.contains(2)) morphisms(2) else Map.empty

    lazy val megamorphicCalls = morphisms - 1 - 2

    def log()(implicit ctx: Context): Unit = {
      ctx.log(s"\t Found: ${classesWithReachableMethods.size} classes with reachable methods, ${reachableClasses.size} reachable classes, ${reachableDefs.size} reachable methods, ${reachableSpecs.size} specializations")
      ctx.log(s"\t mono: ${monomorphicCalls.size}, bi: ${bimorphicCalls.size}, mega: ${megamorphicCalls.map(_._2.size).sum}")
      ctx.log(s"\t Found ${outerMethods.size} not defined calls: ${outerMethods.map(_.showFullName)}")
      ctx.log(s"\t Reachable classes: ${reachableClasses.mkString(", ")}")
      ctx.log(s"\t Reachable methods: ${reachableDefs.map(x => if (x.is(Flags.Module)) x else x.owner.name + "." + x.name).mkString(", ")}")
      ctx.log(s"\t Reachable method classes: ${classesWithReachableMethods.mkString(", ")}")
      ctx.log(s"\t Reachable specs: ${reachableSpecs.mkString(", ")}")
      ctx.log(s"\t Primary Constructor specs: ${reachableSpecs.filter(_._1.isPrimaryConstructor).map(x => (x._1.showFullName, x._2))}")
    }

  }
}
