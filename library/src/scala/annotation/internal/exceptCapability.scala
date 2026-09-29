package scala.annotation
package internal

/** An annotation that represents a capability  `c.except[T]`,
 *  encoded as `x.type @exceptCapability[T]`
 */
class exceptCapability[T] extends StaticAnnotation
