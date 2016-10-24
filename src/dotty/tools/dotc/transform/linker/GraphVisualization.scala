package dotty.tools.dotc.transform.linker

import dotty.tools.dotc.core.Constants._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Names._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.transform.linker.BuildCallGraph._
import dotty.tools.dotc.transform.linker.Summaries._

object GraphVisualization {

  def outputGraph(mode: Int, specLimit: Int)(reachableMethods: Set[CallWithContext],
                  reachableTypes: Set[TypeWithContext],
                  casts: Set[Cast],
                  outerMethod: Set[Symbol])(implicit ctx: Context): String = {
    val classesWithReachableMethods = reachableMethods.map( _.call.termSymbol.maybeOwner.info.widen.classSymbol)
    val reachableClasses = classesWithReachableMethods ++ reachableTypes.flatMap(x => x.tp.classSymbols).flatMap(_.baseClasses)
    val reachableDefs = reachableMethods.map(_.call.termSymbol)

    /*val filter = scala.io.Source.fromFile("trace-filtered").getLines().toList
    /val filterUnMangled = filter.map(x => x.replace("::", ".").replace("$class", "")).toSet

    def fil(x: Symbol) =
      filterUnMangled.contains(x.fullName.toString)

    val liveDefs = reachableDefs.filter{x => fil(x)}     */


    val reachableSpecs: Set[(Symbol, List[Type])] = reachableMethods.flatMap { x =>
      val clas = x.call.termSymbol.maybeOwner.info.widen.classSymbol
      val meth = x.call.termSymbol
      if (mode >= AnalyseTypes) (meth, x.call.normalizedPrefix.baseArgInfos(clas)) :: Nil
      else {
        val clazSpecializationsCount =
          if (clas.primaryConstructor.info.widenDealias.isInstanceOf[PolyType]) specLimit
          else 1
        val methodSpecializationsCount =
          if (meth.info.widenDealias.isInstanceOf[PolyType]) specLimit
          else 1
        println(s"specializing $clas $meth for $clazSpecializationsCount * $methodSpecializationsCount")
        (0 until clazSpecializationsCount*methodSpecializationsCount).map(x => (meth, ConstantType(Constant(x)):: Nil)).toList

      }
    }

    val morphisms = reachableMethods.groupBy(x => x.callee).groupBy(x => x._2.map(_.call.termSymbol).toSet.size)

    val mono = if (morphisms.contains(1)) morphisms(1) else Map.empty
    val bi = if (morphisms.contains(2)) morphisms(2) else Map.empty
    val mega = morphisms - 1 - 2

    println(s"\t Found: ${classesWithReachableMethods.size} classes with reachable methods, ${reachableClasses.size} reachable classes, ${reachableDefs.size} reachable methods, ${reachableSpecs.size} specializations")
    println(s"\t mono: ${mono.size}, bi: ${bi.size}, mega: ${mega.map(_._2.size).sum}")
    println(s"\t Found ${outerMethod.size} not defined calls: ${outerMethod.map(_.showFullName)}")
    println(s"\t Reachable classes: ${classesWithReachableMethods.mkString(", ")}")
    println(s"\t Reachable methods: ${reachableDefs.mkString(", ")}")
    println(s"\t Reachable specs: ${reachableSpecs.mkString(", ")}")
    println(s"\t Primary Constructor specs: ${reachableSpecs.filter(_._1.isPrimaryConstructor).map(x => (x._1.showFullName, x._2))}")

    val outGraph = new StringBuffer()
    outGraph.append(s"digraph Gr${mode}_$specLimit {\n")
    outGraph.append("graph [fontsize=10 fontname=\"Verdana\" compound=true];\n")
    outGraph.append("label = \"" + reachableMethods.size + " nodes, " +
        reachableMethods.foldLeft(0)(_ + _.outEdges.values.foldLeft(0)(_ + _.size)) + " edges, " + reachableTypes.size  + " reachable types\";\n")

    // add names and subraphs
    reachableMethods.foreach { caller =>
      val subgraphColor = if (outerMethod.contains(caller.call.termSymbol)) "red" else "blue"

      outGraph.append("subgraph ").append(clusterName(caller)).append(" {\n")
      outGraph.append("  label = ").append(slash + csWTToName(caller) + slash).append(";\n")
      outGraph.append("  color = ").append(subgraphColor).append(";\n")
      outGraph.append("  ").append(dummyName(caller)).append(" [shape=point style=invis];\n")
      for (call <- caller.outEdges.keys) {
        outGraph.append("  ").append(csToName(caller, call))
        outGraph.append(" [")
        outGraph.append("label=").append(slash).append(callSiteLabel(call)).append(slash)
        if (call.source != null) {
          outGraph.append(", color=")
          val color =
            if (call.source.call.widenDealias.classSymbol.is(JavaDefined)) "orange"
            else "blue"
          outGraph.append(color)
        }
        outGraph.append("];\n")
      }
      outGraph.append("}\n\n")
    }

    // Add edges
    reachableMethods.foreach { caller =>
      caller.outEdges.foreach { x =>
        val callInfo = x._1
        x._2.foreach { target =>
          val from = csToName(caller, callInfo)
          val to = dummyName(target)
          outGraph.append(from).append(" -> ").append(to)
          outGraph.append(" [")
          outGraph.append("ltail=").append(clusterName(target))
          outGraph.append("];\n")
        }
        outGraph.append("\n")
      }
    }
    outGraph.append("}")
    outGraph.toString
  }



  private def callSiteLabel(x: CallInfo)(implicit ctx: Context): String = {
    val prefix = x.call.normalizedPrefix
    val calleeSymbol = x.call.termSymbol
    val prefixString = prefix match {
      case NoPrefix => calleeSymbol.name.toString
      case t if calleeSymbol.isPrimaryConstructor => calleeSymbol.showFullName
      case st: SuperType => s"super[${st.supertpe.classSymbol.showFullName}].${calleeSymbol.name}"
      /* case t if calleeSymbol.is(SuperAccessor) =>
         val prev = t.classSymbol
         types.flatMap {
           x =>
             val s = x.baseClasses.dropWhile(_ != prev)
             if (s.nonEmpty) {
               val parent = s.find(x => x.info.decl(calleeSymbol.name).altsWith(x => x.signature == calleeSymbol.signature).nonEmpty)
               parent match {
                 case Some(p) if p.exists =>
                   val method = p.info.decl(calleeSymbol.name).altsWith(x => x.signature == calleeSymbol.signature)
                   // todo: outerTargs are here defined in terms of location of the subclass. Is this correct?
                   new CallWithContext(t.select(method.head.symbol), targs, args, outerTargs) :: Nil
                 case _ => Nil
               }
             } else Nil
         }     */

      case thisType: ThisType =>
        "this." + calleeSymbol.name
      case t =>
        typeName(t) + '.' + calleeSymbol.name
    }

    val targsString = typeArgumentsString(x.targs)
    val vargsString = argumentsString(x.argumentsPassed)

    prefixString + targsString + vargsString
  }

  private val slash = '"'

  private def escape(s: String) = s.replace("\\", "\\\\").replace("\"","\\\"")

  private def fullNameSeparated(symbol: Symbol)(separator: String)(implicit ctx: Context): Name = {
    var sep = separator
    val owner = symbol.owner
    var name: Name = symbol.name
    var stopAtPackage = false
    if (sep.isEmpty) {
      sep = "$"
      stopAtPackage = true
    }
    if (symbol.isAnonymousClass || symbol.isAnonymousFunction)
      name = name ++ symbol.id.toString
    if (symbol == NoSymbol ||
      owner == NoSymbol ||
      owner.isEffectiveRoot ||
      stopAtPackage && owner.is(PackageClass)) name
    else {
      var encl = owner
      while (!encl.isClass && !encl.isPackageObject) {
        encl = encl.owner
        sep += "~"
      }
      if (owner.is(ModuleClass, butNot = Package) && sep == "$") sep = "" // duplicate scalac's behavior: don't write a double '$$' for module class members.
      val fn = fullNameSeparated(encl)(separator) ++ sep ++ name
      if (symbol.isType) fn.toTypeName else fn.toTermName
    }
  }

  private def symbolName(sym: Symbol)(implicit ctx: Context): String = {
    if (!sym.is(Method)) escape(sym.name.show)
    else escape(fullNameSeparated(sym)(".").show)
  }

  private def typeName(x: Type)(implicit ctx: Context): String = {
    x match {
      case ConstantType(value) => escape(value.toString)
      case _ =>
        val t = x.termSymbol.orElse(x.typeSymbol)
        if (t.exists)
          symbolName(t)
        else escape(x.show)
    }
  }

  private def csWTToName(x: CallInfo)(implicit ctx: Context): String = {
    val targs = typeArgumentsString(x.targs)
    val vargs = typeParameterString(x.call.widenDealias.paramTypess)
    val resultType = typeName(x.call.widenDealias.finalResultType)
    if (x.call.termSymbol.owner == x.call.normalizedPrefix.classSymbol) {
      val callTypeName = typeName(x.call)
      callTypeName + targs + vargs + ": " + resultType
    } else {
      val callTypeName = typeName(x.call.normalizedPrefix)
      val symName = symbolName(x.call.termSymbol)
      callTypeName + ".super." + symName + targs + vargs + ": " + resultType
    }
  }

  private def csToName(parent: CallWithContext, inner: CallInfo)(implicit ctx: Context): String = {
    slash + csWTToName(parent) + escape(inner.call.show) + inner.hashCode() + slash
  }

  private def dummyName(x: CallInfo)(implicit ctx: Context): String = {
    slash + csWTToName(x) + "_Dummy" + slash
  }

  private def clusterName(x: CallInfo)(implicit ctx: Context): String = {
    slash + "cluster_" + csWTToName(x) + slash
  }

  private def argumentsString(args: List[Type])(implicit ctx: Context): String = {
    if (args.isEmpty) ""
    else args.map(typeName).mkString("(", ",", ")")
  }

  private def typeArgumentsString(targs: List[Type])(implicit ctx: Context): String = {
    if (targs.isEmpty) ""
    else targs.map(t => typeName(t.widenDealias)).mkString("[", ",", "]")
  }

  private def typeParameterString(paramTypess: List[List[Type]])(implicit ctx: Context): String = {
    paramTypess.iterator.map { paramTypes =>
      paramTypes.map(x => typeName(x)).mkString("(", ",", ")")
    }.mkString("")
  }
}
