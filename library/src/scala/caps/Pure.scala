package scala
package caps
import annotation.experimental

/** A marker trait that declares that all inheriting classes are "pure" in the
 *  sense that their values retain no capabilities including capabilities needed
 *  to perform effects. This has formal meaning only under capture checking.
 */
@experimental trait Pure:
  this: Pure =>
