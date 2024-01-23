package scala.annotation
package internal

/** An annotation that marks a capture ref as a maybe capability.
 *  `x?` is encoded as `x.type @maybeCapability`
 */
class maybeCapability extends StaticAnnotation

