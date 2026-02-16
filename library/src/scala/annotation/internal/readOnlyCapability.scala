package scala.annotation
package internal

import language.experimental.captureChecking

/** An annotation that marks a capture ref as a read-only capability.
 *  `x.rd` is encoded as `x.type @readOnlyCapability`
 */
class readOnlyCapability extends StaticAnnotation
