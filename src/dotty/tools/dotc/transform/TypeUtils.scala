package dotty.tools.dotc
package transform

import core._
import Types._
import Contexts._
import Symbols._
import Decorators._
import StdNames.nme
import NameOps._
import language.implicitConversions

import scala.language.implicitConversions

object TypeUtils {
  implicit def decorateTypeUtils(tpe: Type): TypeUtils = new TypeUtils(tpe)
}

/** A decorator that provides methods for type transformations
 *  that are needed in the transofmer pipeline (not needed right now)
 */
class TypeUtils(val self: Type) extends AnyVal {

  def caseAccessors(implicit ctx:Context) = self.decls.filter(x => x.is(Flags.CaseAccessor) && x.is(Flags.Method)).toList

}