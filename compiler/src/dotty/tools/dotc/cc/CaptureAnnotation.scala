package dotty.tools
package dotc
package cc

import core.*
import Types.*, Symbols.*, Contexts.*, Annotations.*
import ast.Trees.*
import ast.{tpd, untpd}
import Decorators.*
import config.Printers.capt
import printing.Printer
import printing.Texts.Text

/** An annotation representing a capture set and whether it is boxed.
 *  It simulates a normal @retains annotation except that it is more efficient,
 *  supports variables as capture sets, and adds a `boxed` flag.
 *  These annotations are created during capture checking. Before that
 *  there are only regular @retains and @retainsByName annotations.
 *  @param refs    the capture set
 *  @param boxed   whether the type carrying the annotation is boxed
 *  @param cls     the underlying class (either annotation.retains or annotation.retainsByName)
 */
case class CaptureAnnotation(refs: CaptureSet, boxed: Boolean)(cls: Symbol) extends Annotation:
  import CaptureAnnotation.*
  import tpd.*

  /** A cache for the version of this annotation which differs in its boxed status. */
  var boxDual: CaptureAnnotation | Null = null

  /** A boxed annotation which is either the same annotation or its boxDual */
  def boxedAnnot(using Context): CaptureAnnotation =
    if boxed then this
    else if boxDual != null then boxDual.nn
    else
      val dual = CaptureAnnotation(refs, boxed = true)(cls)
      dual.boxDual = this
      dual

  /** Reconstitute annotation tree from capture set */
  override def tree(using Context) =
    val elems = refs.elems.toList.map {
      case cr: TermRef => ref(cr)
      case cr: TermParamRef => untpd.Ident(cr.paramName).withType(cr)
      case cr: ThisType => This(cr.cls)
      case root(_) => ref(root.cap)
      // TODO: Will crash if the type is an annotated type, for example `cap.rd`
    }
    val arg = repeated(elems, TypeTree(defn.AnyType))
    New(symbol.typeRef, arg :: Nil)

  override def symbol(using Context) = cls

  override def derivedAnnotation(tree: Tree)(using Context): Annotation = this

  def derivedAnnotation(refs: CaptureSet, boxed: Boolean)(using Context): Annotation =
    if (this.refs eq refs) && (this.boxed == boxed) then this
    else CaptureAnnotation(refs, boxed)(cls)

  override def sameAnnotation(that: Annotation)(using Context): Boolean = that match
    case CaptureAnnotation(refs, boxed) =>
      this.refs == refs && this.boxed == boxed && this.symbol == that.symbol
    case _ => false

  override def mapWith(tm: TypeMap)(using Context) =
    val elems = refs.elems.toList
    val elems1 = elems.mapConserve(tm)
    if elems1 eq elems then this
    else if elems1.forall(_.isTrackableRef)
    then derivedAnnotation(CaptureSet(elems1.asInstanceOf[List[CaptureRef]]*), boxed)
    else EmptyAnnotation

  override def refersToParamOf(tl: TermLambda)(using Context): Boolean =
    refs.elems.exists {
      case TermParamRef(tl1, _) => tl eq tl1
      case _ => false
    }

  override def toText(printer: Printer): Text = refs.toText(printer)

  override def hash: Int =
    (refs.hashCode << 1) | (if boxed then 1 else 0)

  override def eql(that: Annotation) = that match
    case that: CaptureAnnotation => (this.refs eq that.refs) && (this.boxed == that.boxed)
    case _ => false

end CaptureAnnotation
