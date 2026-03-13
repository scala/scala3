package scala.annotation
package internal

/** An annotation that represents a capability  `c.only[T]`,
 *  encoded as `x.type @onlyCapability[T]`
 *
 *  @tparam T the capability type that `c` is restricted to via `c.only[T]`
 */
class onlyCapability[T] extends StaticAnnotation

