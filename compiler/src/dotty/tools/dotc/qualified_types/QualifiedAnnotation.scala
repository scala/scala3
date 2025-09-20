package dotty.tools.dotc.qualified_types

import dotty.tools.dotc.ast.tpd.Tree
import dotty.tools.dotc.core.Annotations.Annotation
import dotty.tools.dotc.core.Contexts.{ctx, Context}
import dotty.tools.dotc.core.Decorators.i
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Types.{TermLambda, TermParamRef, Type, ConstantType, TypeMap}
import dotty.tools.dotc.printing.Printer
import dotty.tools.dotc.printing.Texts.Text
import dotty.tools.dotc.printing.Texts.stringToText
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.report

case class QualifiedAnnotation(qualifier: ENode.Lambda) extends Annotation:

  override def tree(using Context): Tree = qualifier.toTree()

  override def symbol(using Context) = defn.QualifiedAnnot

  override def derivedAnnotation(tree: Tree)(using Context): Annotation = ???

  private def derivedAnnotation(qualifier: ENode.Lambda)(using Context): Annotation =
    if qualifier eq this.qualifier then this
    else QualifiedAnnotation(qualifier)

  override def toText(printer: Printer): Text =
    "with " ~ qualifier.body.toString()

  override def mapWith(tm: TypeMap)(using Context): Annotation =
    derivedAnnotation(qualifier.mapTypes(tm).asInstanceOf[ENode.Lambda])

  override def refersToParamOf(tl: TermLambda)(using Context): Boolean =
    var res = false
    qualifier.foreachType: tp =>
      tp.stripped match
        case TermParamRef(tl1, _) if tl eq tl1 => res = true
        case _ => ()
    res

object QualifiedAnnotation:
  def apply(annot: Annotation)(using Context): Annotation =
    annot match
      case QualifiedAnnotation(qualifier) => annot // Already a QualifiedAnnotation
      case _ =>
        val arg = annot.arguments(0)
        ENode.fromTree(arg) match
          case Some(qualifier: ENode.Lambda) => QualifiedAnnotation(qualifier)
          case _ =>
            report.error(i"Invalid qualifier: $arg", annot.tree.srcPos)
            // Returns placeholder (arg0: Any) => true
            QualifiedAnnotation(
              ENode.Lambda(
                List(ENodeParamRef(0, defn.AnyType)),
                defn.BooleanType,
                ENode.Atom(ConstantType(Constant(true)))
              ): ENode.Lambda
            )
