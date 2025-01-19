package scala.annotation
package internal

/** An annotation used internally for fresh capability wrappers of `cap`.
 *  A fresh capability is encoded as `caps.cap @freshCapability(...)` where
 *  `freshCapability(...)` is a special kind of annotation of type `Fresh.Annot`
 *  that contains a hidden set.
 */
class freshCapability extends StaticAnnotation

