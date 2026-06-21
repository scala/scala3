package dotty.tools.dotc
package transform

import core.*
import ast.tpd.*
import Contexts.*
import MegaPhase.*
import Annotations.*
import Symbols.defn
import Constants.*
import Types.*
import Decorators.*
import Flags.*

import scala.collection.mutable

class RepeatableAnnotations extends MiniPhase:

  override def phaseName: String = RepeatableAnnotations.name

  override def description: String = RepeatableAnnotations.description

  override def transformTypeDef(tree: TypeDef)(using Context): Tree = transformDef(tree)
  override def transformValDef(tree: ValDef)(using Context): Tree = transformDef(tree)
  override def transformDefDef(tree: DefDef)(using Context): Tree = transformDef(tree)

  private def transformDef(tree: DefTree)(using Context) =
    val annotations = tree.symbol.annotations
    if (!annotations.isEmpty) then
      tree.symbol.annotations = aggregateAnnotations(tree.symbol.annotations)
    tree

  private def aggregateAnnotations(annotations: Seq[Annotation])(using Context): Vector[Annotation] =
    val annsByType = stableGroupBy(annotations, _.symbol)
    annsByType.flatMap {
      case (_, a +: Vector()) => a +: Vector()
      case (sym, anns) if sym.is(JavaDefined) =>
        sym.getAnnotation(defn.JavaRepeatableAnnot).flatMap(_.argumentConstant(0)) match
          case Some(Constant(containerTpe: Type)) =>
            val clashingAnns = annsByType.getOrElse(containerTpe.classSymbol, Vector())
            if clashingAnns.nonEmpty then
              // this is the same error javac would raise in this case
              val pos = clashingAnns.head.tree.srcPos
              report.error("Container must not be present at the same time as the element it contains", pos)
              Vector()
            else
              val aggregated = JavaSeqLiteral(anns.map(_.tree).toVector, TypeTree(sym.typeRef))
              Annotation(containerTpe, NamedArg("value".toTermName, aggregated), sym.span) +: Vector()
          case _ =>
            val pos = anns.head.tree.srcPos
            report.error("Not repeatable annotation repeated", pos)
            Vector()
      case (_, anns) => anns
    }.toVector

  private def stableGroupBy[A, K](ins: Seq[A], f: A => K): scala.collection.MapView[K, Vector[A]] =
    val out = new mutable.LinkedHashMap[K, mutable.ListBuffer[A]]()
    for (in <- ins) {
      val buffer = out.getOrElseUpdate(f(in), new mutable.ListBuffer)
      buffer += in
    }
    out.view.mapValues(_.toVector)

object RepeatableAnnotations:
  val name: String = "repeatableAnnotations"
  val description: String = "aggregate repeatable annotations"
