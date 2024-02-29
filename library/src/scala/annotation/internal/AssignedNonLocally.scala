package scala.annotation
package internal

/** An annotation to indicate that a private `var` was assigned with a prefix
 *  other than the `this` type of its owner.
 */
class AssignedNonLocally() extends Annotation
