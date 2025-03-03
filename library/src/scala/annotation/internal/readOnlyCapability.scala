package scala.annotation
package internal

/** An annotation that marks a capture ref as a read-only capability.
 *  `x.rd` is encoded as `x.type @readOnlyCapability`
 */
class readOnlyCapability extends StaticAnnotation
