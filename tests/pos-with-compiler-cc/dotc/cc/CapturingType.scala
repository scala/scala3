package dotty.tools
package dotc
package cc

import core.*
import Types.*, Symbols.*, Contexts.*

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

  /** Smart constructor that drops empty capture sets and fuses compatible capturiong types.
   *  An outer type capturing type A can be fused with an inner capturing type B if their
   *  boxing status is the same or if A is boxed.
   */
  def apply(parent: Type, refs: CaptureSet, boxed: Boolean = false)(using Context): Type =
    if refs.isAlwaysEmpty then parent
    else parent match
      case parent @ CapturingType(parent1, refs1) if boxed || !parent.isBoxed =>
        apply(parent1, refs ++ refs1, boxed)
      case _ =>
        AnnotatedType(parent, CaptureAnnotation(refs, boxed)(defn.RetainsAnnot))

  /** An extractor that succeeds only during CheckCapturingPhase. Boxing statis is
   *  returned separately by CaptureOps.isBoxed.
   */
  def unapply(tp: AnnotatedType)(using Context): Option[(Type, CaptureSet)] =
    if ctx.phase == Phases.checkCapturesPhase
      && tp.annot.symbol == defn.RetainsAnnot
      && !ctx.mode.is(Mode.IgnoreCaptures)
    then
      EventuallyCapturingType.unapply(tp)
    else None

end CapturingType

/** An extractor for types that will be capturing types at phase CheckCaptures. Also
 *  included are types that indicate captures on enclosing call-by-name parameters
 *  before phase ElimByName.
 */
object EventuallyCapturingType:

  def unapply(tp: AnnotatedType)(using Context): Option[(Type, CaptureSet)] =
    val sym = tp.annot.symbol
    if sym == defn.RetainsAnnot || sym == defn.RetainsByNameAnnot then
      tp.annot match
        case ann: CaptureAnnotation =>
          Some((tp.parent, ann.refs))
        case ann =>
          try Some((tp.parent, ann.tree.toCaptureSet))
          catch case ex: IllegalCaptureRef => None
    else None

end EventuallyCapturingType


