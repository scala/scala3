package dotty.tools.dotc
package transform

import core._
import ast.tpd._
import Contexts._
import MegaPhase._
import Annotations._
import Symbols.defn
import Constants._
import Types._
import Decorators._

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

  private def aggregateAnnotations(annotations: Seq[Annotation])(using Context): List[Annotation] =
    val annsByType = annotations.groupBy(_.symbol)
    annsByType.flatMap {
      case (_, a :: Nil) => a :: Nil
      case (sym, anns) if sym.derivesFrom(defn.ClassfileAnnotationClass) =>
        sym.getAnnotation(defn.JavaRepeatableAnnot).flatMap(_.argumentConstant(0)) match
          case Some(Constant(containerTpe: Type)) =>
            val clashingAnns = annsByType.getOrElse(containerTpe.classSymbol, Nil)
            if clashingAnns.nonEmpty then
              // this is the same error javac would raise in this case
              val pos = clashingAnns.head.tree.srcPos
              report.error("Container must not be present at the same time as the element it contains", pos)
              Nil
            else
              val aggregated = JavaSeqLiteral(anns.map(_.tree).toList, TypeTree(sym.typeRef))
              Annotation(containerTpe, NamedArg("value".toTermName, aggregated)) :: Nil
          case _ =>
            val pos = anns.head.tree.srcPos
            report.error("Not repeatable annotation repeated", pos)
            Nil
      case (_, anns) => anns
    }.toList

object RepeatableAnnotations:
  val name: String = "repeatableAnnotations"
  val description: String = "aggregate repeatable annotations"
