package dotty.tools.dotc.transform.linker.callgraph

import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types._

import scala.collection.mutable

class CallGraph(val entryPoints: Map[CallInfoWithContext, Int], val reachableMethods: Set[CallInfoWithContext],
    val reachableTypes: Set[TypeWithContext], val casts: Set[Cast], val classOfs: Set[Symbol],
    val outerMethods: Set[Symbol], val mode: Int, val specLimit: Int)(implicit ctx: Context) { self =>

  private val reachableMethodSet: Set[Symbol] =
    reachableMethods.map(_.callSymbol)

  lazy val reachableClassesSet: Set[Symbol] = {
    val reachableClasses = mutable.Set.empty[Symbol]
    def collectClasses(cls: ClassSymbol): Unit = {
      reachableClasses.add(cls)
      cls.classParents.foreach(x => collectClasses(x.symbol.asClass))
    }
    reachableTypes.foreach(x => x.tp.classSymbols.foreach(collectClasses))
    reachableClasses.toSet
  }

  def isReachableMethod(sym: Symbol): Boolean = reachableMethodSet.contains(sym)

  def isReachableClass(sym: Symbol): Boolean = reachableClassesSet.contains(sym)

  def isReachableClassOf(sym: Symbol): Boolean = classOfs.contains(sym)

  def getInfo(): Info = new Info

  class Info {

    lazy val classesWithReachableMethods = reachableMethodSet.map(_.maybeOwner.info.widen.classSymbol)

    lazy val reachableClasses = classesWithReachableMethods ++ reachableClassesSet

    lazy val reachableDefs = reachableMethods.map(_.callSymbol)

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
      ctx.log(
        s"""
           |Call graph info:
           |  Found: ${classesWithReachableMethods.size} classes with reachable methods, ${reachableClasses.size} reachable classes, ${reachableDefs.size} reachable methods, ${reachableSpecs.size} specializations
           |    mono: ${monomorphicCalls.size}, bi: ${bimorphicCalls.size}, mega: ${megamorphicCalls.map(_._2.size).sum}
           |  Found ${outerMethods.size} not defined calls: ${outerMethods.map(_.showFullName)}
           |  Reachable classes: ${reachableClasses.map(_.showFullName).mkString(", ")}
           |  Reachable methods: ${reachableDefs.map(_.showFullName).mkString(", ")}
           |  Classes with reachable methods: ${classesWithReachableMethods.map(_.showFullName).mkString(", ")}
           |  Reachable specs: ${reachableSpecs.toList.filter(_._2.nonEmpty).sortBy(-_._2.size).map(x => (x._1.showFullName, x._2.map(_.show))).mkString(", ")}
           |  Primary Constructor specs: ${reachableSpecs.filter(x => x._1.isPrimaryConstructor && x._2.nonEmpty).map(x => (x._1.showFullName, x._2))}
         """.stripMargin
      )
    }

  }
}
