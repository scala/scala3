package dotty.tools.dotc.transform.linker

import dotty.tools.dotc.core.Constants.Constant
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

    val slash = '"'

    def escape(s: String) = s.replace("\\", "\\\\").replace("\"","\\\"")

    def fullNameSeparated(symbol: Symbol)(separator: String)(implicit ctx: Context): Name = {
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

    def symbolName(s: Symbol): String = {
      if (!s.is(Method))
        escape(s.name.show)
      else
        escape(fullNameSeparated(s)(".").show)
    }

    def typeName(x: Type): String = {
      x match {
        case ConstantType(value) => s"${escape(value.toString)}"
        case _ =>
          val t = x.termSymbol.orElse(x.typeSymbol)
          if (t.exists)
            symbolName(t)
          else escape(x.show)
      }
    }

    def csWTToName(x: CallInfo, close: Boolean = true, open: Boolean = true): String = {
      if (x.call.termSymbol.owner == x.call.normalizedPrefix.classSymbol) {
        s"${if (open) slash else ""}${typeName(x.call)}${if (x.targs.nonEmpty) "[" + x.targs.map(x => typeName(x)).mkString(",") + "]" else ""}${if (close) slash else ""}"
      } else {
        s"${if (open) slash else ""}${typeName(x.call.normalizedPrefix)}.super.${symbolName(x.call.termSymbol)}${if (x.targs.nonEmpty) "[" + x.targs.map(x => typeName(x)).mkString(",") + "]" else ""}${if (close) slash else ""}"
      }
    }

    def csToName(parrent: CallWithContext, inner: CallInfo): String = {
      csWTToName(parrent, close = false) + s"${escape(inner.call.show)}${inner.hashCode()}$slash"
    }

    def dummyName(x: CallInfo) = {
      csWTToName(x, close = false) + "_Dummy\""
    }

    def clusterName(x: CallInfo) = {
      val r =  "\"cluster_" + csWTToName(x, open = false)
//      if (r.contains("BufferLike.apply")) {
//        println("doba")
//      }
      r
    }

    // add names and subraphs
    reachableMethods.foreach { caller =>
      def callSiteLabel(x: CallInfo): String = {
        val prefix = x.call.normalizedPrefix
        val calleeSymbol = x.call.termSymbol
        prefix match {
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

          case thisType: ThisType => s"this.${calleeSymbol.name}"
          case t =>
            s"${typeName(t)}.${calleeSymbol.name}"
        }
      }

      val line = s"""subgraph ${clusterName(caller)} {
          label = ${csWTToName(caller)}; color = ${if (outerMethod.contains(caller.call.termSymbol)) "red" else "blue"};
          ${dummyName(caller)} [shape=point style=invis];
        ${caller.outEdges.keys.map(x => csToName(caller, x) + s" [label = $slash${callSiteLabel(x)}$slash${if (x.source == null) "" else if (x.source.call.widenDealias.classSymbol.is(JavaDefined)) ", color=orange" else ", color=blue"}];").mkString("")}
        }
        """
      outGraph.append(line)
      caller.outEdges.foreach { x =>
        val callInfo = x._1
        x._2.foreach { target =>
          val line = s"${csToName(caller, callInfo)} -> ${dummyName(target)} [ltail=${clusterName(target)}];\n"
          outGraph.append(line)
        }
//        if (callInfo.source != null) {
//          val line = s"${csToName(caller, callInfo)} -> ${dummyName(callInfo.source)} [dir=back, color=orange];\n"
//          outGraph.append(line)
//        }
      }
    }
    outGraph.append("}")
    outGraph.toString
  }

}
