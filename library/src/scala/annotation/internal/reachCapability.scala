package scala.annotation
package internal

import language.experimental.captureChecking

/** An annotation that marks a capture ref as a reach capability.
 *  `x*` is encoded as `x.type @reachCapability`
 */
@deprecated("Reach capabilities are no longer supported", since = "3.10")
class reachCapability extends StaticAnnotation

