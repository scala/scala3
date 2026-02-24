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
    qctx.reflect.Symbol.requiredPackage("scala.caps.cap")
  def Caps_Capability(using qctx: Quotes) =
    qctx.reflect.Symbol.requiredClass("scala.caps.Capability")
  def Caps_CapSet(using qctx: Quotes) =
    qctx.reflect.Symbol.requiredClass("scala.caps.CapSet")
  def Caps_Mutable(using qctx: Quotes) =
    qctx.reflect.Symbol.requiredClass("scala.caps.Mutable")
  def Caps_SharedCapability(using qctx: Quotes) =
    qctx.reflect.Symbol.requiredClass("scala.caps.SharedCapability")
  def UseAnnot(using qctx: Quotes) =
    qctx.reflect.Symbol.requiredClass("scala.caps.use")
  def ConsumeAnnot(using qctx: Quotes) =
    qctx.reflect.Symbol.requiredClass("scala.caps.internal.consume")
  def ReachCapabilityAnnot(using qctx: Quotes) =
    qctx.reflect.Symbol.requiredClass("scala.annotation.internal.reachCapability")
  def RootCapabilityAnnot(using qctx: Quotes) =
    qctx.reflect.Symbol.requiredClass("scala.caps.internal.rootCapability")
  def ReadOnlyCapabilityAnnot(using qctx: Quotes) =
    qctx.reflect.Symbol.requiredClass("scala.annotation.internal.readOnlyCapability")
  def RequiresCapabilityAnnot(using qctx: Quotes) =
    qctx.reflect.Symbol.requiredClass("scala.annotation.internal.requiresCapability")
  def OnlyCapabilityAnnot(using qctx: Quotes) =
    qctx.reflect.Symbol.requiredClass("scala.annotation.internal.onlyCapability")

  def LanguageExperimental(using qctx: Quotes) =
    qctx.reflect.Symbol.requiredPackage("scala.language.experimental")

  def ImpureFunction1(using qctx: Quotes) =
    qctx.reflect.Symbol.requiredClass("scala.ImpureFunction1")

  def ImpureContextFunction1(using qctx: Quotes) =
    qctx.reflect.Symbol.requiredClass("scala.ImpureContextFunction1")

  def Function1(using qctx: Quotes) =
    qctx.reflect.Symbol.requiredClass("scala.Function1")

  def ContextFunction1(using qctx: Quotes) =
    qctx.reflect.Symbol.requiredClass("scala.ContextFunction1")

  val useAnnotFullName: String = "scala.caps.use.<init>"
  val consumeAnnotFullName: String = "scala.caps.consume.<init>"
  val ccImportSelector = "captureChecking"
end CaptureDefs

extension (using qctx: Quotes)(ann: qctx.reflect.Symbol)
  /** This symbol is one of `retains` or `retainsCap` */
  def isRetains: Boolean =
    ann == CaptureDefs.retains || ann == CaptureDefs.retainsCap

  /** This symbol is one of `retains`, `retainsCap`, or `retainsByName` */
  def isRetainsLike: Boolean =
    ann.isRetains || ann == CaptureDefs.retainsByName

  def isReachCapabilityAnnot: Boolean =
    ann == CaptureDefs.ReachCapabilityAnnot

  def isReadOnlyCapabilityAnnot: Boolean =
    ann == CaptureDefs.ReadOnlyCapabilityAnnot

  def isOnlyCapabilityAnnot: Boolean =
    ann == CaptureDefs.OnlyCapabilityAnnot
end extension

extension (using qctx: Quotes)(tpe: qctx.reflect.TypeRepr) // FIXME clean up and have versions on Symbol for those
  def isCaptureRoot: Boolean =
    import qctx.reflect.*
    tpe match
      case TermRef(ThisType(TypeRef(NoPrefix(), "caps")), "cap") => true
      case TermRef(TermRef(ThisType(TypeRef(NoPrefix(), "scala")), "caps"), "cap") => true
      case TermRef(TermRef(TermRef(TermRef(NoPrefix(), "_root_"), "scala"), "caps"), "cap") => true
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

/** Matches `import scala.language.experimental.captureChecking` */
object CCImport:
  def unapply(using qctx: Quotes)(tree: qctx.reflect.Tree): Boolean =
    import qctx.reflect._
    tree match
      case imprt: Import if imprt.expr.tpe.termSymbol == CaptureDefs.LanguageExperimental =>
        imprt.selectors.exists {
          case SimpleSelector(s) if s == CaptureDefs.ccImportSelector => true
          case _ => false
        }
      case _ => false
  end unapply
end CCImport

object ReachCapability:
  def unapply(using qctx: Quotes)(ty: qctx.reflect.TypeRepr): Option[qctx.reflect.TypeRepr] =
    import qctx.reflect._
    ty match
      case AnnotatedType(base, Apply(Select(New(annot), _), Nil)) if annot.symbol.isReachCapabilityAnnot =>
        Some(base)
      case _ => None
end ReachCapability

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
      case t @ ReachCapability(_)    => include(t)
      case t @ ReadOnlyCapability(_) => include(t)
      case t @ OnlyCapability(_, _)  => include(t)
      case t : TypeRef               => include(t)
      case _ => report.warning(s"Unexpected type tree $typ while trying to extract capture references from $typ0"); false
  if traverse(typ0) then Some(buffer.toList) else None
end decomposeCaptureRefs

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
