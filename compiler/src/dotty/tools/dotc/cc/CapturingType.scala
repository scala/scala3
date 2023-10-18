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
   *   - drops a capability class expansion if it is further refined with another capturing type
   *   - fuses compatible capturing types.
   *  An outer type capturing type A can be fused with an inner capturing type B if their
   *  boxing status is the same or if A is boxed.
   */
  def apply(parent: Type, refs: CaptureSet, boxed: Boolean = false)(using Context): Type =
    if refs.isAlwaysEmpty then parent
    else parent match
      case parent @ CapturingType(parent1, refs1) if refs1 eq defn.expandedUniversalSet =>
        apply(parent1, refs, boxed)
      case parent @ CapturingType(parent1, refs1) if boxed || !parent.isBoxed =>
        apply(parent1, refs ++ refs1, boxed)
      case _ =>
        AnnotatedType(parent, CaptureAnnotation(refs, boxed)(defn.RetainsAnnot))

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
  def decomposeCapturingType(tp: Type)(using Context): Option[(Type, CaptureSet)] = tp match
    case AnnotatedType(parent, ann: CaptureAnnotation)
    if isCaptureCheckingOrSetup =>
      Some((parent, ann.refs))
    case AnnotatedType(parent, ann)
    if ann.symbol == defn.RetainsAnnot && isCaptureChecking =>
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
      try Some((parent, ann.tree.toCaptureSet))
      catch case ex: IllegalCaptureRef => None
    case _ =>
      None

  /** Check whether a type is uncachable when computing `baseType`.
   *  We avoid caching capturing types when IgnoreCaptures mode is set.
   */
  def isUncachable(tp: Type)(using Context): Boolean =
    ctx.mode.is(Mode.IgnoreCaptures) && decomposeCapturingType(tp).isDefined

end CapturingType


