package dotty.tools
package dotc
package transform

import core._
import TypeErasure.ErasedValueType
import Types._
import Contexts._
import Symbols._
import Names.Name

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

    def widenToParents(implicit ctx: Context): Type = self.parents match {
      case Nil => self
      case ps => ps.reduceLeft(AndType(_, _))
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

    def refinedWith(name: Name, info: Type)(implicit ctx: Context) = RefinedType(self, name, info)

    /** The TermRef referring to the companion of the underlying class reference
     *  of this type, while keeping the same prefix.
     */
    def companionRef(implicit ctx: Context): TermRef = self match {
      case self @ TypeRef(prefix, _) if self.symbol.isClass =>
        prefix.select(self.symbol.companionModule).asInstanceOf[TermRef]
      case self: TypeProxy =>
        self.underlying.companionRef
    }
  }
}
