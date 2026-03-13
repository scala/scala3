package scala.annotation
package internal

import language.experimental.captureChecking

/** An annotation that represents a capability  `c.only[T]`,
 *  encoded as `x.type @onlyCapability[T]`
 */
class onlyCapability[T] extends StaticAnnotation

