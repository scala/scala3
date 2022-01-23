package dotty.tools
package dotc
package cc

import core.*
import Types.*, Symbols.*, Contexts.*, Annotations.*
import ast.{tpd, untpd}
import Decorators.*
import config.Printers.capt
import util.Property.Key
import tpd.*

private val Captures: Key[CaptureSet] = Key()
private val IsBoxed: Key[Unit] = Key()

def retainedElems(tree: Tree)(using Context): List[Tree] = tree match
  case Apply(_, Typed(SeqLiteral(elems, _), _) :: Nil) => elems
  case _ => Nil

class IllegalCaptureRef(tpe: Type) extends Exception

extension (tree: Tree)

  def toCaptureRef(using Context): CaptureRef = tree.tpe match
    case ref: CaptureRef => ref
    case tpe => throw IllegalCaptureRef(tpe)

  def toCaptureSet(using Context): CaptureSet =
    tree.getAttachment(Captures) match
      case Some(refs) => refs
      case None =>
        val refs = CaptureSet(retainedElems(tree).map(_.toCaptureRef)*)
          .showing(i"toCaptureSet $tree --> $result", capt)
        tree.putAttachment(Captures, refs)
        refs

  def isBoxedCapturing(using Context): Boolean =
    tree.hasAttachment(IsBoxed)

  def setBoxedCapturing()(using Context): Unit =
    tree.putAttachment(IsBoxed, ())

extension (tp: Type)

  def derivedCapturingType(parent: Type, refs: CaptureSet)(using Context): Type = tp match
    case CapturingType(p, r, k) =>
      if (parent eq p) && (refs eq r) then tp
      else CapturingType(parent, refs, k)

  /** If this is  type variable instantiated or upper bounded with a capturing type,
   *  the capture set associated with that type. Extended to and-or types and
   *  type proxies in the obvious way. If a term has a type with a boxed captureset,
   *  that captureset counts towards the capture variables of the envirionment.
   */
  def boxedCaptured(using Context): CaptureSet =
    def getBoxed(tp: Type): CaptureSet = tp match
      case CapturingType(_, refs, CapturingKind.Boxed) => refs
      case CapturingType(_, _, _) => CaptureSet.empty
      case tp: TypeProxy => getBoxed(tp.superType)
      case tp: AndType => getBoxed(tp.tp1) ++ getBoxed(tp.tp2)
      case tp: OrType => getBoxed(tp.tp1) ** getBoxed(tp.tp2)
      case _ => CaptureSet.empty
    getBoxed(tp)

  def isBoxedCapturing(using Context) = !tp.boxedCaptured.isAlwaysEmpty

  def stripCapturing(using Context): Type = tp.dealiasKeepAnnots match
    case CapturingType(parent, _, _) =>
      parent.stripCapturing
    case atd @ AnnotatedType(parent, annot) =>
      atd.derivedAnnotatedType(parent.stripCapturing, annot)
    case _ =>
      tp
