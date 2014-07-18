package dotty.tools.dotc
package transform

import dotty.tools.dotc.core.Types._

import scala.language.implicitConversions

object TypeUtils {
  implicit def decorateTypeUtils(tpe: Type): TypeUtils = new TypeUtils(tpe)

}

/** A decorator that provides methods for type transformations
 *  that are needed in the transofmer pipeline (not needed right now)
 */
class TypeUtils(val self: Type) extends AnyVal {

}