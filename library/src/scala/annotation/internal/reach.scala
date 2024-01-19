package scala.annotation
package internal

/** An annotation that marks a capture ref as a reach capability.
 *  `x*` is encoded as `x.type @reachCapability`
 */
class reachCapability extends StaticAnnotation

