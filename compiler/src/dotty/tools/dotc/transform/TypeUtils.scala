package dotty.tools
package dotc
package transform

import core._
import TypeErasure.ErasedValueType
import Types._
import Contexts._
import Symbols._

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

    /** The arity of this tuple type, which can be made up of Unit, TupleX and `*:` pairs,
     *  or -1 if this is not a tuple type.
     */
    def tupleArity(implicit ctx: Context): Int = self match {
      case AppliedType(tycon, _ :: tl :: Nil) if tycon.isRef(defn.PairClass) =>
        val arity = tl.tupleArity
        if (arity < 0) arity else arity + 1
      case tp1 =>
        if (tp1.isRef(defn.UnitClass)) 0
        else if (defn.isTupleClass(tp1.classSymbol)) tp1.dealias.argInfos.length
        else -1
    }

    /** The element types of this tuple type, which can be made up of Unit, TupleX and `*:` pairs */
    def tupleElementTypes(implicit ctx: Context): List[Type] = self match {
      case AppliedType(tycon, hd :: tl :: Nil) if tycon.isRef(defn.PairClass) =>
        hd :: tl.tupleElementTypes
      case tp1 =>
        if (tp1.isRef(defn.UnitClass)) Nil
        else if (defn.isTupleClass(tp1.classSymbol)) tp1.dealias.argInfos
        else throw new AssertionError("not a tuple")
    }

    /** The `*:` equivalent of an instance of a Tuple class */
    def toNestedPairs(implicit ctx: Context): Type =
      TypeOps.nestedPairs(tupleElementTypes)

    /** Extract opaque alias from TypeBounds type that combines it with the reference
     *  to the opaque type itself
     */
    def extractOpaqueAlias(implicit ctx: Context): Type = self match {
      case TypeBounds(lo, _) =>
        def extractAlias(tp: Type): Type = tp match {
          case OrType(alias, _) => alias
          case self: HKTypeLambda => self.derivedLambdaType(resType = extractAlias(self.resType))
        }
        extractAlias(lo)
    }
  }
}
