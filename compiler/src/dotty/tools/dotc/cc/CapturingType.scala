package dotty.tools
package dotc
package cc

import core.*
import Types.*, Symbols.*, Contexts.*
import Decorators.i

/** A (possibly boxed) capturing type. This is internally represented as an annotated type with a @retains
 *  or @retainsByName annotation, but the extractor will succeed only at phase CheckCaptures.
 *  That way, we can ignore caturing information until phase CheckCaptures since it is
 *  wrapped in a plain annotation.
 *
 *  The same trick does not work for the boxing information. Boxing is context dependent, so
 *  we have to add that information in the Setup step preceding CheckCaptures. Boxes are
 *  added for all type arguments of methods. For type arguments of applied types a different
 *  strategy is used where we box arguments of applied types that are not functions when
 *  accessing the argument.
 *
 *  An alternative strategy would add boxes also to arguments of applied types during setup.
 *  But this would have to be done for all possibly accessibly types from the compiled units
 *  as well as their dependencies. It's difficult to do this in a DenotationTransformer without
 *  accidentally forcing symbol infos. That's why this alternative was not implemented.
 *  If we would go back on this it would make sense to also treat captuyring types different
 *  from annotations and to generate them all during Setup and in DenotationTransformers.
 */
object CapturingType:

  /** Smart constructor that
   *   - drops empty capture sets
   *   - fuses compatible capturing types.
   *  An outer type capturing type A can be fused with an inner capturing type B if their
   *  boxing status is the same or if A is boxed.
   */
  def apply(parent: Type, refs: CaptureSet, boxed: Boolean = false)(using Context): Type =
    assert(!boxed || !parent.derivesFrom(defn.Caps_CapSet))
    if refs.isAlwaysEmpty && !refs.keepAlways && !parent.derivesFromCapability then
      parent
    else parent match
      case parent @ CapturingType(parent1, refs1) if boxed || !parent.isBoxed =>
        apply(parent1, refs ++ refs1, boxed)
      case _ =>
        val refs1 =
          if parent.derivesFromStateful then refs.associateWithStateful() else refs
        refs1.adoptClassifier(parent.inheritedClassifier)
        AnnotatedType(parent, CaptureAnnotation(refs1, boxed)(defn.RetainsAnnot))

  /** An extractor for CapturingTypes. Capturing types are recognized if
   *   - the annotation is a CaptureAnnotation and we are not past CheckCapturingPhase, or
   *   - the annotation is a @retains and we are in CheckCapturingPhase,
   *  but not if the IgnoreCaptures mode is set.
   *  Boxing status is returned separately by CaptureOps.isBoxed.
   */
  def unapply(tp: AnnotatedType)(using Context): Option[(Type, CaptureSet)] =
    if ctx.mode.is(Mode.IgnoreCaptures) then None
    else decomposeCapturingType(tp)

  /** Decompose `tp` as a capturing type without taking IgnoreCaptures into account */
  def decomposeCapturingType(using Context)(tp: Type, alsoRetains: Boolean = isCaptureChecking): Option[(Type, CaptureSet)] = tp match
    case AnnotatedType(parent, ann: CaptureAnnotation)
    if isCaptureCheckingOrSetup =>
      Some((parent, ann.refs))
    case AnnotatedType(parent, ann: RetainingAnnotation) if ann.isStrict && alsoRetains =>
      // There are some circumstances where we cannot map annotated types
      // with retains annotations to capturing types, so this second recognizer
      // path still has to exist. One example is when checking capture sets
      // of dependent function type results for well-formedness. E.g. in
      // `(x: C^{f}) -> () ->{x} Unit` we need to check that the capture set of
      // `x` is not empty. We use the original, untransformed type for that
      // since the transformed type already normalizes capture sets which would
      // drop subsumed references. But the original type refers to the untransfomed
      // type `C^{f}` which does not have a capture annotation yet. The transformed
      // type would be in a copy of the dependent function type, but it is useless
      // since we need to check the original reference.
      //
      // TODO In other situations we expect that the type is already transformed to a
      // CapturingType and we should crash if this not the case.
      try Some((parent, ann.toCaptureSet))
      catch case ex: IllegalCaptureRef => None
    case _ =>
      None

  /** Check whether a type is uncachable when computing `baseType`.
   *  We avoid caching capturing types when IgnoreCaptures mode is set.
   */
  def isUncachable(tp: Type)(using Context): Boolean =
    ctx.mode.is(Mode.IgnoreCaptures) && decomposeCapturingType(tp).isDefined

end CapturingType

object CapturingOrRetainsType:
   def unapply(tp: AnnotatedType)(using Context): Option[(Type, CaptureSet)] =
    if ctx.mode.is(Mode.IgnoreCaptures) then None
    else CapturingType.decomposeCapturingType(tp, alsoRetains = true)


