package dotty.tools.scaladoc

package cc

import scala.quoted._

import dotty.tools.dotc.core.{Contexts, Symbols, Types}
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.NameOps.*
import dotty.tools.dotc.cc.RetainingAnnotation
import dotty.tools.dotc.{cc => dcc}

// Scaladoc inspects TASTy with the compiler's implementation of the reflect API,
// so its types and symbols are the compiler's own. The capture checking support
// below defers to the compiler's definitions instead of duplicating them.

private[scaladoc] def inCompiler[T](using qctx: Quotes)(op: Contexts.Context ?=> T): T =
  op(using qctx.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx)

private def toType(using qctx: Quotes)(tp: qctx.reflect.TypeRepr): Types.Type = tp.asInstanceOf[Types.Type]
private def fromType(using qctx: Quotes)(tp: Types.Type): qctx.reflect.TypeRepr = tp.asInstanceOf[qctx.reflect.TypeRepr]
private def toSymbol(using qctx: Quotes)(sym: qctx.reflect.Symbol): Symbols.Symbol = sym.asInstanceOf[Symbols.Symbol]
private def fromSymbol(using qctx: Quotes)(sym: Symbols.Symbol): qctx.reflect.Symbol = sym.asInstanceOf[qctx.reflect.Symbol]

object CaptureDefs:
  def Caps_Capability(using qctx: Quotes): qctx.reflect.Symbol = inCompiler(fromSymbol(defn.Caps_Capability))
  def Caps_CapSet(using qctx: Quotes): qctx.reflect.Symbol = inCompiler(fromSymbol(defn.Caps_CapSet))
  def Caps_any(using qctx: Quotes): qctx.reflect.Symbol = inCompiler(fromSymbol(defn.Caps_any))
  def ConsumeAnnot(using qctx: Quotes): qctx.reflect.Symbol = inCompiler(fromSymbol(defn.ConsumeAnnot))

  /** Is `sym` one of `FunctionN` or `ContextFunctionN`, or one of the type aliases
   *  `ImpureFunctionN` and `ImpureContextFunctionN` by which the impure function types
   *  `A => B` and `A ?=> B` are written under capture checking? The test is by name,
   *  without dealiasing.
   */
  def isFunctionClass(using qctx: Quotes)(sym: qctx.reflect.Symbol): Boolean =
    inCompiler(defn.isFunctionSymbol(toSymbol(sym)))

  def isContextFunctionClass(using qctx: Quotes)(sym: qctx.reflect.Symbol): Boolean =
    inCompiler(isFunctionClass(sym) && toSymbol(sym).name.isContextFunction)

  def isImpureFunctionClass(using qctx: Quotes)(sym: qctx.reflect.Symbol): Boolean =
    inCompiler(isFunctionClass(sym) && toSymbol(sym).name.isImpureFunction)
end CaptureDefs

extension (using qctx: Quotes)(sym: qctx.reflect.Symbol)
  /** Is this one of the annotation classes `retains` or `retainsCap`? */
  def isRetains: Boolean = inCompiler(dcc.isRetains(toSymbol(sym)))

  /** Was the class enclosing this symbol compiled with capture checking? Unpickling
   *  sets the `CaptureChecked` flag on the classes of capture-checked TASTy files, so
   *  this holds per defining class, no matter how capture checking was enabled.
   */
  def isCaptureChecked: Boolean = inCompiler:
    val dsym = toSymbol(sym)
    dsym.exists && dsym.enclosingClass.is(dotty.tools.dotc.core.Flags.CaptureChecked)
end extension

extension (using qctx: Quotes)(tpe: qctx.reflect.TypeRepr)
  /** Is this a direct reference to `scala.caps.any`, the universal capability? Does not
   *  look through capability wrappers such as `any.rd`.
   */
  def isCaptureRoot: Boolean = inCompiler:
    toType(tpe) match
      case tp: Types.TermRef => tp.symbol == defn.Caps_any
      case _ => false

  /** Is this a direct reference to `scala.caps.fresh`, the existentially bound
   *  capability of function results (see scoped-capabilities.md)?
   */
  def isFreshCap: Boolean = inCompiler:
    toType(tpe) match
      case tp: Types.TermRef => tp.symbol == defn.Caps_fresh
      case _ => false

  /** Like `dealiasKeepOpaques`, but keeps annotations. Under capture checking, an alias
   *  such as `type F[A] = A => B` expands to `ImpureFunction1[A, B]` and further to
   *  `Function1[A, B]^`, so dropping annotations would make the function type pure.
   */
  def dealiasKeepAnnotsAndOpaques: qctx.reflect.TypeRepr =
    inCompiler(fromType(toType(tpe).dealiasKeepAnnotsAndOpaques))

  def isCapSet: Boolean = inCompiler(toType(tpe).typeSymbol == defn.Caps_CapSet)

  def isCapSetPure: Boolean =
    tpe.isCapSet && tpe.match
      case CapturingType(_, refs) => refs.isEmpty
      case _ => true

  def isCapSetCap: Boolean =
    tpe.isCapSet && tpe.match
      case CapturingType(_, List(ref)) => ref.isCaptureRoot
      case _ => false

  /** Is this a type whose values never retain capabilities? Resolves the type to a class
   *  relative to `from` and asks the capture checker whether it is pure. Outside of capture
   *  checking, that errs on the side of impure for classes whose purity depends on an
   *  explicit self type without a capture set.
   */
  def isPureClass(from: qctx.reflect.ClassDef): Boolean =
    // Approximates tpe.asSeenFrom(from) to resolve abstract types and aliases.
    val tpe1 = from.symbol.typeRef.select(tpe.typeSymbol).simplified
    inCompiler(dcc.isPureClass(toSymbol(tpe1.classSymbol.getOrElse(tpe1.typeSymbol))))
end extension

extension (using qctx: Quotes)(typedef: qctx.reflect.TypeDef)
  def derivesFromCapSet: Boolean =
    import qctx.reflect.*
    typedef.rhs.match
      case t: TypeTree => t.tpe.derivesFrom(CaptureDefs.Caps_CapSet)
      case t: TypeBoundsTree => t.tpe.derivesFrom(CaptureDefs.Caps_CapSet)
      case _ => false
end extension

/** The elements of the capture set carried by a retaining annotation, with the
 *  union-type encoding of capture sets flattened.
 */
private def retainedRefs(using qctx: Quotes)(ann: RetainingAnnotation)(using Contexts.Context): List[qctx.reflect.TypeRepr] =
  dcc.retainedElementsRaw(ann.retainedType).map(fromType)

/** The elements of the capture set of the annotation `annot`, if it is a `retains` or
 *  `retainsCap` annotation.
 */
def retainedCaptureRefs(using qctx: Quotes)(annot: qctx.reflect.Term): Option[List[qctx.reflect.TypeRepr]] =
  inCompiler:
    val annotType = toType(annot.tpe)
    Option.when(dcc.isRetains(annotType.typeSymbol))(retainedRefs(RetainingAnnotation(annotType)))

/** Matches a type with a retained capture set, `T^{refs}`, encoded as `T` annotated with
 *  `retains` or `retainsCap`.
 */
object CapturingType:
  def unapply(using qctx: Quotes)(typ: qctx.reflect.TypeRepr): Option[(qctx.reflect.TypeRepr, List[qctx.reflect.TypeRepr])] =
    inCompiler:
      toType(typ) match
        case Types.AnnotatedType(parent, ann: RetainingAnnotation) if dcc.isRetains(ann.symbol) =>
          Some((fromType(parent), retainedRefs(ann)))
        case _ => None
end CapturingType

/** Matches the result type of a by-name type with a capture set on its arrow,
 *  `->{refs} T` or `=> T`, which is encoded as `T @retainsByName[refs]`. Unlike
 *  `CapturingType`, it does not match a by-name result type that captures, `-> T^{refs}`.
 */
object ByNameCapturingType:
  def unapply(using qctx: Quotes)(typ: qctx.reflect.TypeRepr): Option[(qctx.reflect.TypeRepr, List[qctx.reflect.TypeRepr])] =
    inCompiler:
      toType(typ) match
        case Types.AnnotatedType(result, ann: RetainingAnnotation) if ann.symbol == defn.RetainsByNameAnnot =>
          Some((fromType(result), retainedRefs(ann)))
        case _ => None
end ByNameCapturingType

/** Matches a read-only capability `ref.rd` and returns `ref`. */
object ReadOnlyCapability:
  def unapply(using qctx: Quotes)(ty: qctx.reflect.TypeRepr): Option[qctx.reflect.TypeRepr] =
    inCompiler:
      toType(ty) match
        case tp: Types.AnnotatedType => dcc.ReadOnlyCapability.unapply(tp).map(fromType)
        case _ => None
end ReadOnlyCapability

/** Matches a restricted capability `ref.only[C]` and returns `ref` and the classifier `C`. */
object OnlyCapability:
  def unapply(using qctx: Quotes)(ty: qctx.reflect.TypeRepr): Option[(qctx.reflect.TypeRepr, qctx.reflect.Symbol)] =
    inCompiler:
      toType(ty) match
        case tp: Types.AnnotatedType =>
          dcc.OnlyCapability.unapply(tp).map((ref, cls) => (fromType(ref), fromSymbol(cls)))
        case _ => None
end OnlyCapability

/** Matches an excluded capability `ref.except[C]` and returns `ref` and the classifier `C`. */
object ExceptCapability:
  def unapply(using qctx: Quotes)(ty: qctx.reflect.TypeRepr): Option[(qctx.reflect.TypeRepr, qctx.reflect.Symbol)] =
    inCompiler:
      toType(ty) match
        case tp: Types.AnnotatedType =>
          dcc.ExceptCapability.unapply(tp).map((ref, cls) => (fromType(ref), fromSymbol(cls)))
        case _ => None
end ExceptCapability
