package dotty.tools.scaladoc
package tasty

import scala.quoted._

/** Scaladoc-specific helpers for rendering capture-checked signatures.
 *
 *  The extractors for capturing types and capabilities live in the experimental
 *  `cc` module of the quotes reflect API: see `cc.CapturingType`,
 *  `cc.ReadOnlyCapability`, `cc.OnlyCapability`, `cc.RetainingAnnotation`, as
 *  well as `Symbol.isPureClass` and the capture-checking members of `defn`.
 *  This file only keeps glue that is specific to how scaladoc renders
 *  capture-checked code.
 */
object CaptureDefs:
  def LanguageExperimental(using qctx: Quotes) =
    qctx.reflect.Symbol.requiredPackage("scala.language.experimental")
  val ccImportSelector = "captureChecking"
end CaptureDefs

extension (using qctx: Quotes)(tpe: qctx.reflect.TypeRepr)
  /** Is this a direct reference to `scala.caps.any`, the universal capability `cap`?
   *  Deliberately does not look through capability wrappers such as `any.rd` or
   *  `any.only[C]`, which are annotated types over the underlying reference.
   */
  def isCaptureRoot: Boolean =
    import qctx.reflect._
    tpe match
      case tpe: TermRef => tpe.termSymbol == defn.Caps_any
      case _ => false

  /** Is this a direct reference to `scala.caps.fresh`, the existentially-bound
   *  capability of function type results (see scoped-capabilities.md)?
   */
  def isFreshCap: Boolean =
    import qctx.reflect._
    tpe match
      case tpe: TermRef => tpe.termSymbol == defn.Caps_fresh
      case _ => false

  def isCapSet: Boolean = tpe.typeSymbol == qctx.reflect.defn.Caps_CapSet

  def isCapSetPure: Boolean =
    import qctx.reflect._
    tpe.isCapSet && tpe.match
      case cc.CapturingType(_, refs) => refs.isEmpty
      case _ => true

  def isCapSetCap: Boolean =
    import qctx.reflect._
    tpe.isCapSet && tpe.match
      case cc.CapturingType(_, List(ref)) => ref.isCaptureRoot
      case _ => false

  /** Is this a type whose values never retain capabilities? Resolves the type
   *  to a class relative to `from` and defers to `Symbol.isPureClass`.
   */
  def isPureClass(from: qctx.reflect.ClassDef): Boolean =
    import qctx.reflect._
    // Approximates tpe.asSeenFrom(from) to resolve abstract types and aliases.
    val tpe1 = from.symbol.typeRef.select(tpe.typeSymbol).simplified
    tpe1.classSymbol.getOrElse(tpe1.typeSymbol).isPureClass
end extension

extension (using qctx: Quotes)(typedef: qctx.reflect.TypeDef)
  def derivesFromCapSet: Boolean =
    import qctx.reflect.*
    typedef.rhs.match
      case t: TypeTree => t.tpe.derivesFrom(defn.Caps_CapSet)
      case t: TypeBoundsTree => t.tpe.derivesFrom(defn.Caps_CapSet)
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
