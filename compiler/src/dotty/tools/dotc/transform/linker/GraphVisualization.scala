package dotty.tools.dotc.transform.linker

import dotty.tools.dotc.core.Constants._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Names._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.transform.linker.BuildCallGraph._
import dotty.tools.dotc.transform.linker.Summaries._
import strawman.collections.CollectionStrawMan4.ListBuffer

import scala.collection.mutable

object GraphVisualization {

  def outputDiagnostic(mode: Int, specLimit: Int)(callGraph: CallGraph)(implicit ctx: Context): String = {
    val output = new StringBuffer()

    val reachableMethods = callGraph.reachableMethods
    val reachableTypes = callGraph.reachableTypes
    val outerMethod = callGraph.outerMethods

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
        output.append(s"specializing $clas $meth for $clazSpecializationsCount * $methodSpecializationsCount")
        (0 until clazSpecializationsCount*methodSpecializationsCount).map(x => (meth, ConstantType(Constant(x)):: Nil)).toList

      }
    }

    val morphisms = reachableMethods.groupBy(x => x.callee).groupBy(x => x._2.map(_.call.termSymbol).toSet.size)

    val mono = if (morphisms.contains(1)) morphisms(1) else Map.empty
    val bi = if (morphisms.contains(2)) morphisms(2) else Map.empty
    val mega = morphisms - 1 - 2

    output.append(s"\t Found: ${classesWithReachableMethods.size} classes with reachable methods, ${reachableClasses.size} reachable classes, ${reachableDefs.size} reachable methods, ${reachableSpecs.size} specializations")
    output.append(s"\t mono: ${mono.size}, bi: ${bi.size}, mega: ${mega.map(_._2.size).sum}")
    output.append(s"\t Found ${outerMethod.size} not defined calls: ${outerMethod.map(_.showFullName)}")
    output.append(s"\t Reachable classes: ${classesWithReachableMethods.mkString(", ")}")
    output.append(s"\t Reachable methods: ${reachableDefs.mkString(", ")}")
    output.append(s"\t Reachable specs: ${reachableSpecs.mkString(", ")}")
    output.append(s"\t Primary Constructor specs: ${reachableSpecs.filter(_._1.isPrimaryConstructor).map(x => (x._1.showFullName, x._2))}")

    output.toString
  }

  def outputGraph(mode: Int, specLimit: Int)(callGraph: CallGraph)(implicit ctx: Context): String = {
    val reachableMethods = callGraph.reachableMethods
    val reachableTypes = callGraph.reachableTypes
    val outerMethod = callGraph.outerMethods

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

  def outputGraphVis(mode: Int, specLimit: Int)(callGraph: CallGraph)(implicit ctx: Context): String = {
    val reachableMethods = callGraph.reachableMethods
    val reachableTypes = callGraph.reachableTypes
    val outerMethod = callGraph.outerMethods

    // add names and subraphs
    val nodes = mutable.Map.empty[String, String]
    val edges = mutable.Map.empty[String, List[String]]

    val red = "'rgb(255,150,150)'"
    val blue = "'rgb(150,150,255)'"
    val green = "'rgb(150,255,150)'"

    reachableMethods.foreach { caller =>

      val color =
        if (outerMethod.contains(caller.call.termSymbol)) red
        else blue

      def mkId(callWithContext: CallWithContext): String = {
        callWithContext.call.uniqId.toString
      }

      val callerId = s"'${mkId(caller)}'"
      nodes(callerId) = s"{ id: $callerId, label: '${csWTToShortName(caller)}', title: '${csWTToName(caller)}', color: $color }"

      if (caller.isEntryPoint) {
        val entryId = s"'entry-${mkId(caller)}'"
        nodes(entryId) = s"{ id: $entryId, shape: 'diamond', color: $green }"
        edges(entryId) = s"{ from: $entryId, to: $callerId }" :: edges.getOrElse(entryId, Nil)
      } /* else if (caller.callee == null) {
        val entryId = s"'entry-${mkId(caller)}'"
        nodes(entryId) = s"{ id: $entryId, hidden: true }"
        edges(entryId) = s"{ from: $entryId, to: $callerId, hidden: true }" :: edges.getOrElse(entryId, Nil)
      } */

      caller.outEdges.iterator.foreach {
        case (call, callees) =>
          callees.foreach { callee =>
            val calleeId = s"'${mkId(callee)}'"
            edges(callerId) = s"{ from: $callerId, to: $calleeId, title: '${callSiteLabel(call)}' }" :: edges.getOrElse(callerId, Nil)
          }
      }
    }

    visHTML(nodes.toMap, edges.toMap)
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

  private def csWTToShortName(x: CallInfo)(implicit ctx: Context): String = {
    if (x.call.termSymbol.owner == x.call.normalizedPrefix.classSymbol.name) {
      val callTypeName = typeName(x.call)
      callTypeName
    } else {
      val callTypeName = typeName(x.call.normalizedPrefix)
      symbolName(x.call.termSymbol)
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


  private def visHTML(nodes: Map[String, String], edges: Map[String, List[String]]): String = {
    val nodesJSON = nodes.iterator.map{ case (key, v) => key + ": " + v}.mkString("{\n      ", ",\n      ", "\n    }")
    val edgesJSON = edges.iterator.map{ case (key, ns) => key + ": " + ns.mkString("[", ",", "]")}.mkString("{\n      ", ",\n      ", "\n    }")
    s"""
      |<!doctype html>
      |<html>
      |<head>
      |  <title>Callgraph</title>
      |
      |  <script type="text/javascript" src="https://cdnjs.cloudflare.com/ajax/libs/vis/4.16.1/vis.min.js"></script>
      |  <link href="https://cdnjs.cloudflare.com/ajax/libs/vis/4.16.1/vis.min.css" rel="stylesheet" type="text/css">
      |  <style type="text/css">
      |    #mynetwork {
      |      width: 1080px;
      |      height: 720px;
      |      border: 1px solid lightgray;
      |    }
      |    p {
      |      max-width:1000px;
      |    }
      |  </style>
      |</head>
      |
      |<body>
      |<div id="wrapper">
      |  <div id="mynetwork"></div>
      |  <div id="loadingBar">
      |    <div class="outerBorder">
      |      <div id="text">0%</div>
      |      <div id="border">
      |        <div id="bar"></div>
      |      </div>
      |    </div>
      |  </div>
      |</div>
      |
      |
      |<script type="text/javascript">
      |  var container = document.getElementById('mynetwork');
      |  var options = {
      |    layout: {
      |      improvedLayout: true
      |    },
      |    edges: {
      |      smooth: true,
      |      arrows: { to: true }
      |    },
      |    interaction: {
      |      hover: true
      |    },
      |    physics: {
      |      enabled: true,
      |      solver: 'repulsion',
      |      maxVelocity: 10,
      |      minVelocity: 1,
      |      repulsion: {
      |        nodeDistance: 400
      |      }
      |    }
      |  };
      |
      |  var nodesMap = $nodesJSON
      |  var edgesMap = $edgesJSON
      |
      |  var data = {
      |    nodes: new vis.DataSet([]),
      |    edges: new vis.DataSet([])
      |  };
      |
      |  var doubleClickOnNode = function (id) {
      |    var edgs = edgesMap[id];
      |    var node = nodesMap[id];
      |    if (!id.startsWith('entry') && !node.isLeaf) {
      |      if (node.shape != 'box') {
      |        node.shape = 'box';
      |        var edges1 = edgesMap[id];
      |        for (i in edges1)
      |          data.edges.add(edges1[i]);
      |      } else {
      |        node.shape = undefined;
      |        var edges1 = edgesMap[id];
      |        for (i in edges1)
      |          data.edges.remove(edges1[i]);
      |      }
      |      data.nodes.update(nodesMap[id]);
      |    }
      |    for (e in edgs) {
      |      var nodeId = edgs[e].to
      |      var node = nodesMap[nodeId];
      |      if (!node.added) {
      |        if (!edgesMap[nodeId]) {
      |          node.shape = 'box';
      |          node.shapeProperties = { borderDashes: [10,5] };
      |          node.isLeaf = true;
      |        }
      |        data.nodes.add(node);
      |        node.added = true;
      |      }
      |    }
      |  };
      |  for (i in nodesMap) {
      |    if (i.startsWith('entry')) {
      |      data.nodes.add(nodesMap[i]);
      |      data.edges.add(edgesMap[i]);
      |      doubleClickOnNode(i);
      |    }
      |  }
      |
      |  var network = new vis.Network(container, data, options);
      |   network.on("stabilizationProgress", function(params) {
      |      var maxWidth = 496;
      |      var minWidth = 20;
      |      var widthFactor = params.iterations/params.total;
      |      var width = Math.max(minWidth,maxWidth * widthFactor);
      |
      |      document.getElementById('bar').style.width = width + 'px';
      |      document.getElementById('text').innerHTML = Math.round(widthFactor*100) + '%';
      |  });
      |  network.once("stabilizationIterationsDone", function() {
      |      document.getElementById('text').innerHTML = '100%';
      |      document.getElementById('bar').style.width = '496px';
      |      document.getElementById('loadingBar').style.opacity = 0;
      |      // really clean the dom element
      |      setTimeout(function () {document.getElementById('loadingBar').style.display = 'none';}, 500);
      |  });
      |  network.on("doubleClick", function (params) {
      |      params.event = "[original event]";
      |      if (params.nodes && params.nodes.length > 0)
      |        doubleClickOnNode(params.nodes[0]);
      |  });
      |</script>
      |
      |</body>
      |</html>
    """.stripMargin
  }
}
