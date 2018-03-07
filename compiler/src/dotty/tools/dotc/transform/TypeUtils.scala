package dotty.tools.dotc
package transform

import core._
import TypeErasure.ErasedValueType
import Types._
import Contexts._
import Symbols._
import Decorators._
import StdNames.nme
import NameOps._
import language.implicitConversions

object TypeUtils {
  /** A decorator that provides methods on types
   *  that are needed in the transformer pipeline.
   */
  implicit class TypeUtilsOps(val self: Type) extends AnyVal {

    def isErasedValueType(implicit ctx: Context): Boolean =
      self.isInstanceOf[ErasedValueType]

    def isPrimitiveValueType(implicit ctx: Context): Boolean =
      self.classSymbol.isPrimitiveValueClass

    def ensureMethodic(implicit ctx: Context): Type = self match {
      case self: MethodicType => self
      case _ => if (ctx.erasedTypes) MethodType(Nil, self) else ExprType(self)
    }
  }
}
