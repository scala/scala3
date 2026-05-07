package dotty.tools.dotc.qualified_types

import dotty.tools.dotc.ast.tpd.Tree
import dotty.tools.dotc.core.Annotations.Annotation
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.{ctx, Context}
import dotty.tools.dotc.core.Decorators.i
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Types.{ConstantType, TermLambda, TermParamRef, Type, TypeMap}
import dotty.tools.dotc.printing.Printer
import dotty.tools.dotc.printing.Texts.{Text, given}
import dotty.tools.dotc.report

case class QualifiedAnnotation(qualifier: ENode.Lambda) extends Annotation:

  // Override hash/eql so that AnnotatedType hash-consing recognises two
  // structurally-equal qualifier annotations as equal. The default
  // Annotation implementation uses reference identity, which would
  // produce distinct AnnotatedType instances for equivalent qualifiers
  // and break ENode structural equality on embedded type-args.
  override def hash: Int = qualifier.hashCode
  override def eql(that: Annotation): Boolean = that match
    case that: QualifiedAnnotation => qualifier == that.qualifier
    case _ => false

  override def tree(using Context): Tree = qualifier.toTree()

  override def symbol(using Context) = defn.QualifiedAnnot

  override def derivedAnnotation(tree: Tree)(using Context): Annotation = ???

  def derivedAnnotation(qualifier: ENode.Lambda)(using Context): Annotation =
    if qualifier eq this.qualifier then this
    else QualifiedAnnotation(qualifier)

  override def toText(printer: Printer): Text =
    "with " ~ qualifier.body.toText(printer)

  override def mapWith(tm: TypeMap)(using Context): Annotation =
    derivedAnnotation(qualifier.mapTypes(tm).asInstanceOf[ENode.Lambda])

  override def refersToParamOf(tl: TermLambda)(using Context): Boolean =
    var res = false
    qualifier.foreachType: tp =>
      tp.stripped match
        case TermParamRef(tl1, _) if tl eq tl1 => res = true
        case _ => ()
    res

  def foldOverTypes[A](z: A, f: (A, Type) => A)(using Context): A =
    var acc = z
    qualifier.foreachType: tp =>
      acc = f(acc, tp)
    acc
