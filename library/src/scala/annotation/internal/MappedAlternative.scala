package scala.annotation
package internal

import language.experimental.captureChecking

/** An annotation added by overloading resoluton to mapped symbols that
 *  explore deeper into the types of the opverloaded alternatives.
 *  Its tree is a TypeTree with two parameters which are both needed to
 *  fine default getters in later parameter sections.
 *  @tparam  Prefix    the prefix field of the original alternative TermRef
 *  @tparam  SkipCount a ConstantType referring to the number of skipped term parameters
 *  The annotation is short-lived since mapped symbols are discarded immediately
 *  once an overloading resolution step terminates.
 */
final class MappedAlternative[Prefix, SkipCount] extends Annotation
