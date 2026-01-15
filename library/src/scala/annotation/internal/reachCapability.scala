package scala.annotation
package internal

import language.experimental.captureChecking

/** An annotation that marks a capture ref as a reach capability.
 *  `x*` is encoded as `x.type @reachCapability`
 */
class reachCapability extends StaticAnnotation

