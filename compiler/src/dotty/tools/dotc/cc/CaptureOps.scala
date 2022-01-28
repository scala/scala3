package dotty.tools
package dotc
package cc

import core.*
import Types.*, Symbols.*, Contexts.*, Annotations.*, Flags.*
import ast.{tpd, untpd}
import Decorators.*, NameOps.*
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

  /** Under -Ycc, map regular function type to impure function type
   */
  def adaptFunctionType(using Context): Type = tp match
    case AppliedType(fn, args)
    if ctx.settings.Ycc.value && defn.isFunctionClass(fn.typeSymbol) =>
      val fname = fn.typeSymbol.name
      defn.FunctionType(
        fname.functionArity,
        isContextual = fname.isContextFunction,
        isErased = fname.isErasedFunction,
        isImpure = true).appliedTo(args)
    case _ =>
      tp

extension (sym: Symbol)

  /** Does this symbol allow results carrying the universal capability?
   *  Currently this is true only for function type applies (since their
   *  results are unboxed) and `erasedValue` since this function is magic in
   *  that is allows to conjure global capabilies from nothing (aside: can we find a
   *  more controlled way to achieve this?).
   *  But it could be generalized to other functions that so that they can take capability
   *  classes as arguments.
   */
  def allowsRootCapture(using Context): Boolean =
    sym == defn.Compiletime_erasedValue
    || defn.isFunctionClass(sym.maybeOwner)

  def unboxesResult(using Context): Boolean =
    def containsEnclTypeParam(tp: Type): Boolean = tp.strippedDealias match
      case tp @ TypeRef(pre: ThisType, _) => tp.symbol.is(Param)
      case tp: TypeParamRef => true
      case tp: AndOrType => containsEnclTypeParam(tp.tp1) || containsEnclTypeParam(tp.tp2)
      case tp: RefinedType => containsEnclTypeParam(tp.parent) || containsEnclTypeParam(tp.refinedInfo)
      case _ => false
    containsEnclTypeParam(sym.info.finalResultType)
    && !sym.allowsRootCapture
