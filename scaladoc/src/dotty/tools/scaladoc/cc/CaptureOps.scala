package dotty.tools.scaladoc

package cc

import scala.quoted._

object CaptureDefs:
  // these should become part of the reflect API in the distant future
  def retains(using qctx: Quotes) =
    qctx.reflect.Symbol.requiredClass("scala.annotation.retains")
  def retainsCap(using qctx: Quotes) =
    qctx.reflect.Symbol.requiredClass("scala.annotation.retainsCap")
  def retainsByName(using qctx: Quotes) =
    qctx.reflect.Symbol.requiredClass("scala.annotation.retainsByName")
  def CapsModule(using qctx: Quotes) =
    qctx.reflect.Symbol.requiredPackage("scala.caps")
  def captureRoot(using qctx: Quotes) =
    qctx.reflect.Symbol.requiredPackage("scala.caps." + captureRootName)
  def freshCap(using qctx: Quotes) =
    qctx.reflect.Symbol.requiredPackage("scala.caps." + freshCapName)
  def Caps_Capability(using qctx: Quotes) =
    qctx.reflect.Symbol.requiredClass("scala.caps.Capability")
  def Caps_CapSet(using qctx: Quotes) =
    qctx.reflect.Symbol.requiredClass("scala.caps.CapSet")
  def Caps_Mutable(using qctx: Quotes) =
    qctx.reflect.Symbol.requiredClass("scala.caps.Mutable")
  def Caps_SharedCapability(using qctx: Quotes) =
    qctx.reflect.Symbol.requiredClass("scala.caps.SharedCapability")
  def ConsumeAnnot(using qctx: Quotes) =
    qctx.reflect.Symbol.requiredClass("scala.caps.internal.consume")
  def ReadOnlyCapabilityAnnot(using qctx: Quotes) =
    qctx.reflect.Symbol.requiredClass("scala.annotation.internal.readOnlyCapability")
  def RequiresCapabilityAnnot(using qctx: Quotes) =
    qctx.reflect.Symbol.requiredClass("scala.annotation.internal.requiresCapability")
  def OnlyCapabilityAnnot(using qctx: Quotes) =
    qctx.reflect.Symbol.requiredClass("scala.annotation.internal.onlyCapability")
  def ExceptCapabilityAnnot(using qctx: Quotes) =
    qctx.reflect.Symbol.requiredClass("scala.annotation.internal.exceptCapability")

  def ImpureFunction1(using qctx: Quotes) =
    qctx.reflect.Symbol.requiredClass("scala.ImpureFunction1")

  def ImpureContextFunction1(using qctx: Quotes) =
    qctx.reflect.Symbol.requiredClass("scala.ImpureContextFunction1")

  def Function1(using qctx: Quotes) =
    qctx.reflect.Symbol.requiredClass("scala.Function1")

  def ContextFunction1(using qctx: Quotes) =
    qctx.reflect.Symbol.requiredClass("scala.ContextFunction1")

  val consumeAnnotFullName: String = "scala.caps.consume.<init>"
  val captureRootName = "any"
  val freshCapName = "fresh"
end CaptureDefs

extension (using qctx: Quotes)(ann: qctx.reflect.Symbol)
  /** This symbol is one of `retains` or `retainsCap` */
  def isRetains: Boolean =
    ann == CaptureDefs.retains || ann == CaptureDefs.retainsCap

  /** This symbol is one of `retains`, `retainsCap`, or `retainsByName` */
  def isRetainsLike: Boolean =
    ann.isRetains || ann == CaptureDefs.retainsByName

  def isReadOnlyCapabilityAnnot: Boolean =
    ann == CaptureDefs.ReadOnlyCapabilityAnnot

  def isOnlyCapabilityAnnot: Boolean =
    ann == CaptureDefs.OnlyCapabilityAnnot

  def isExceptCapabilityAnnot: Boolean =
    ann == CaptureDefs.ExceptCapabilityAnnot
end extension

extension (using qctx: Quotes)(tpe: qctx.reflect.TypeRepr) // FIXME clean up and have versions on Symbol for those
  def isCaptureRoot: Boolean =
    import qctx.reflect.*
    tpe match
      case TermRef(ThisType(TypeRef(NoPrefix(), "caps")), CaptureDefs.captureRootName) => true
      case TermRef(TermRef(ThisType(TypeRef(NoPrefix(), "scala")), "caps"), CaptureDefs.captureRootName) => true
      case TermRef(TermRef(TermRef(TermRef(NoPrefix(), "_root_"), "scala"), "caps"), CaptureDefs.captureRootName) => true
      case _ => false

  // Recognizes `caps.fresh` — the existentially-bound capability for function type
  // results (see scoped-capabilities.md). Analogous to `isCaptureRoot` for `caps.cap`.
  // Matches all prefix variants the compiler may produce in TASTY.
  def isFreshCap: Boolean =
    import qctx.reflect.*
    tpe match
      case TermRef(ThisType(TypeRef(NoPrefix(), "caps")), CaptureDefs.freshCapName) => true
      case TermRef(TermRef(ThisType(TypeRef(NoPrefix(), "scala")), "caps"), CaptureDefs.freshCapName) => true
      case TermRef(TermRef(TermRef(TermRef(NoPrefix(), "_root_"), "scala"), "caps"), CaptureDefs.freshCapName) => true
      case _ => false

  // NOTE: There's something horribly broken with Symbols, and we can't rely on tests like .isContextFunctionType either,
  // so we do these lame string comparisons instead.
  def isImpureFunction1: Boolean = tpe.typeSymbol.fullName == "scala.ImpureFunction1"

  def isImpureContextFunction1: Boolean = tpe.typeSymbol.fullName == "scala.ImpureContextFunction1"

  def isFunction1: Boolean = tpe.typeSymbol.fullName == "scala.Function1"

  def isContextFunction1: Boolean = tpe.typeSymbol.fullName == "scala.ContextFunction1"

  def isAnyImpureFunction: Boolean = tpe.typeSymbol.fullName.startsWith("scala.ImpureFunction")

  def isAnyImpureContextFunction: Boolean = tpe.typeSymbol.fullName.startsWith("scala.ImpureContextFunction")

  def isAnyFunction: Boolean = tpe.typeSymbol.fullName.startsWith("scala.Function")

  def isAnyContextFunction: Boolean = tpe.typeSymbol.fullName.startsWith("scala.ContextFunction")

  def isAnyFunctionType: Boolean =
    tpe.isAnyFunction || tpe.isAnyContextFunction || tpe.isAnyImpureFunction || tpe.isAnyImpureContextFunction

  /** Like `dealiasKeepOpaques`, but keeps annotations. Under capture checking, an alias
   *  such as `type F[A] = A => B` expands to `ImpureFunction1[A, B]` and further to
   *  `Function1[A, B]^`, so dropping annotations would make the function type pure.
   */
  def dealiasKeepAnnotsAndOpaques: qctx.reflect.TypeRepr =
    import dotty.tools.dotc.core.{Contexts, Types}
    given Contexts.Context = qctx.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx
    tpe.asInstanceOf[Types.Type].dealiasKeepAnnotsAndOpaques.asInstanceOf[qctx.reflect.TypeRepr]

  def isCapSet: Boolean = tpe.typeSymbol == CaptureDefs.Caps_CapSet

  def isCapSetPure: Boolean =
    tpe.isCapSet && tpe.match
      case CapturingType(_, refs) => refs.isEmpty
      case _ => true

  def isCapSetCap: Boolean =
    tpe.isCapSet && tpe.match
      case CapturingType(_, List(ref)) => ref.isCaptureRoot
      case _ => false

  def isPureClass(from: qctx.reflect.ClassDef): Boolean =
    import qctx.reflect._
    def check(sym: Tree): Boolean = sym match
      case ClassDef(name, _, _, Some(ValDef(_, tt, _)), _) => tt.tpe match
        case CapturingType(_, refs) => refs.isEmpty
        case _ => true
      case _ => false

    // Horrible hack to basically grab tpe1.asSeenFrom(from)
    val tpe1 = from.symbol.typeRef.select(tpe.typeSymbol).simplified
    val tpe2 = tpe1.classSymbol.map(_.typeRef).getOrElse(tpe1)

    // println(s"${tpe.show} -> (${tpe.typeSymbol} from ${from.symbol}) ${tpe1.show} -> ${tpe2} -> ${tpe2.baseClasses.filter(_.isClassDef)}")
    val res = tpe2.baseClasses.exists(c => c.isClassDef && check(c.tree))
    // println(s"${tpe.show} is pure class = $res")
    res
end extension

extension (using qctx: Quotes)(typedef: qctx.reflect.TypeDef)
  def derivesFromCapSet: Boolean =
    import qctx.reflect.*
    typedef.rhs.match
      case t: TypeTree => t.tpe.derivesFrom(CaptureDefs.Caps_CapSet)
      case t: TypeBoundsTree => t.tpe.derivesFrom(CaptureDefs.Caps_CapSet)
      case _ => false
end extension

extension (using qctx: Quotes)(sym: qctx.reflect.Symbol)
  /** Was the class enclosing this symbol compiled with capture checking? Unpickling
   *  sets the `CaptureChecked` flag on the classes of capture-checked TASTy files, so
   *  this holds per defining class, no matter how capture checking was enabled.
   */
  def isCaptureChecked: Boolean =
    import dotty.tools.dotc.core.{Contexts, Flags, Symbols}
    given Contexts.Context = qctx.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx
    val dsym = sym.asInstanceOf[Symbols.Symbol]
    dsym.exists && dsym.enclosingClass.is(Flags.CaptureChecked)
end extension

object ReadOnlyCapability:
  def unapply(using qctx: Quotes)(ty: qctx.reflect.TypeRepr): Option[qctx.reflect.TypeRepr] =
    import qctx.reflect._
    ty match
      case AnnotatedType(base, Apply(Select(New(annot), _), Nil)) if annot.symbol.isReadOnlyCapabilityAnnot =>
        Some(base)
      case _ => None
end ReadOnlyCapability

object OnlyCapability:
  def unapply(using qctx: Quotes)(ty: qctx.reflect.TypeRepr): Option[(qctx.reflect.TypeRepr, qctx.reflect.Symbol)] =
    import qctx.reflect._
    ty match
      case AnnotatedType(base, app @ Apply(TypeApply(Select(New(annot), _), _), Nil)) if annot.tpe.typeSymbol.isOnlyCapabilityAnnot =>
        app.tpe.typeArgs.head.classSymbol.match
          case Some(clazzsym) => Some((base, clazzsym))
          case None => None
      case _ => None
end OnlyCapability

object ExceptCapability:
  def unapply(using qctx: Quotes)(ty: qctx.reflect.TypeRepr): Option[(qctx.reflect.TypeRepr, qctx.reflect.Symbol)] =
    import qctx.reflect._
    ty match
      case AnnotatedType(base, app @ Apply(TypeApply(Select(New(annot), _), _), Nil)) if annot.tpe.typeSymbol.isExceptCapabilityAnnot =>
        app.tpe.typeArgs.head.classSymbol.match
          case Some(clazzsym) => Some((base, clazzsym))
          case None => None
      case _ => None
end ExceptCapability

/** Decompose capture sets in the union-type-encoding into the sequence of atomic `TypeRepr`s.
 *  Returns `None` if the type is not a capture set.
*/
def decomposeCaptureRefs(using qctx: Quotes)(typ0: qctx.reflect.TypeRepr): Option[List[qctx.reflect.TypeRepr]] =
  import qctx.reflect._
  val buffer = collection.mutable.ListBuffer.empty[TypeRepr]
  def include(t: TypeRepr): Boolean = { buffer += t; true }
  def traverse(typ: TypeRepr): Boolean =
    typ match
      case t if t.typeSymbol == defn.NothingClass => true
      case OrType(t1, t2)            => traverse(t1) && traverse(t2)
      case t @ ThisType(_)           => include(t)
      case t @ TermRef(_, _)         => include(t)
      case t @ ParamRef(_, _)        => include(t)
      case t @ ReadOnlyCapability(_) => include(t)
      case t @ OnlyCapability(_, _)  => include(t)
      case t @ ExceptCapability(_, _) => include(t)
      case t : TypeRef               => include(t)
      case _ => report.warning(s"Unexpected type tree $typ while trying to extract capture references from $typ0"); false
  if traverse(typ0) then Some(buffer.toList) else None
end decomposeCaptureRefs

def retainedCaptureRefs(using qctx: Quotes)(annot: qctx.reflect.Term): Option[List[qctx.reflect.TypeRepr]] =
  import qctx.reflect._
  if annot.tpe.typeSymbol == CaptureDefs.retainsCap then
    Some(CaptureDefs.captureRoot.termRef :: Nil)
  else if annot.tpe.typeSymbol.isRetains then
    annot.tpe.typeArgs match
      case CaptureSetType(refs) :: Nil => Some(refs)
      case _ => None
  else None

object CaptureSetType:
  def unapply(using qctx: Quotes)(tt: qctx.reflect.TypeRepr): Option[List[qctx.reflect.TypeRepr]] = decomposeCaptureRefs(tt)
end CaptureSetType

object CapturingType:
  def unapply(using qctx: Quotes)(typ: qctx.reflect.TypeRepr): Option[(qctx.reflect.TypeRepr, List[qctx.reflect.TypeRepr])] =
    import qctx.reflect._
    typ match
      case AnnotatedType(base, annot) if annot.symbol == CaptureDefs.retainsCap =>
        Some((base, List(CaptureDefs.captureRoot.termRef)))
      case AnnotatedType(base, annot) if annot.tpe.typeSymbol.isRetainsLike =>
        annot.tpe.match
          case AppliedType(_, List(CaptureSetType(refs))) =>
            Some((base, refs))
          case _ =>
            None
      case _ => None
end CapturingType

/** Matches the result type of a by-name type with a capture set on its arrow,
 *  `->{refs} T` or `=> T`, which is encoded as `T @retainsByName[refs]`. Unlike
 *  `CapturingType`, it does not match a by-name result type that captures, `-> T^{refs}`.
 */
object ByNameCapturingType:
  def unapply(using qctx: Quotes)(typ: qctx.reflect.TypeRepr): Option[(qctx.reflect.TypeRepr, List[qctx.reflect.TypeRepr])] =
    import qctx.reflect._
    typ match
      case AnnotatedType(base, annot) if annot.tpe.typeSymbol == CaptureDefs.retainsByName =>
        annot.tpe match
          case AppliedType(_, List(CaptureSetType(refs))) => Some((base, refs))
          case _ => None
      case _ => None
end ByNameCapturingType
