package dotty.tools
package dotc
package transform

import core.*
import TypeErasure.ErasedValueType
import Types.*
import Contexts.*
import Symbols.*
import Names.Name

import dotty.tools.dotc.core.Decorators.*

object TypeUtils {
  /** A decorator that provides methods on types
   *  that are needed in the transformer pipeline.
   */
  extension (self: Type) {

    def isErasedValueType(using Context): Boolean =
      self.isInstanceOf[ErasedValueType]

    def isPrimitiveValueType(using Context): Boolean =
      self.classSymbol.isPrimitiveValueClass

    def isErasedClass(using Context): Boolean =
      self.underlyingClassRef(refinementOK = true).typeSymbol.is(Flags.Erased)

    /** Is this type a checked exception? This is the case if the type
     *  derives from Exception but not from RuntimeException. According to
     *  that definition Throwable is unchecked. That makes sense since you should
     *  neither throw nor catch `Throwable` anyway, so we should not define
     *  a capability to do so.
     */
    def isCheckedException(using Context): Boolean =
      self.derivesFrom(defn.ExceptionClass)
      && !self.derivesFrom(defn.RuntimeExceptionClass)

    def isByName: Boolean =
      self.isInstanceOf[ExprType]

    def ensureMethodic(using Context): Type = self match {
      case self: MethodicType => self
      case _ => if (ctx.erasedTypes) MethodType(Nil, self) else ExprType(self)
    }

    def widenToParents(using Context): Type = self.parents match {
      case Nil => self
      case ps => ps.reduceLeft(AndType(_, _))
    }

    /** The element types of this tuple type, which can be made up of EmptyTuple, TupleX and `*:` pairs */
    def tupleElementTypes(using Context): Option[List[Type]] = self.dealias match {
      case AppliedType(tycon, hd :: tl :: Nil) if tycon.isRef(defn.PairClass) =>
        tl.tupleElementTypes.map(hd :: _)
      case self: SingletonType =>
        if self.termSymbol == defn.EmptyTupleModule then Some(Nil) else None
      case AndType(tp1, tp2) =>
        // We assume that we have the following property:
        // (T1, T2, ..., Tn) & (U1, U2, ..., Un) = (T1 & U1, T2 & U2, ..., Tn & Un)
        tp1.tupleElementTypes.zip(tp2.tupleElementTypes).map { case (t1, t2) => t1.intersect(t2) }
      case OrType(tp1, tp2) =>
        None // We can't combine the type of two tuples
      case _ =>
        if defn.isTupleClass(self.typeSymbol) then Some(self.dealias.argInfos)
        else None
    }

    /** The `*:` equivalent of an instance of a Tuple class */
    def toNestedPairs(using Context): Type =
      tupleElementTypes match
        case Some(types) => TypeOps.nestedPairs(types)
        case None => throw new AssertionError("not a tuple")

    def refinedWith(name: Name, info: Type)(using Context) = RefinedType(self, name, info)

    /** The TermRef referring to the companion of the underlying class reference
     *  of this type, while keeping the same prefix.
     */
    def mirrorCompanionRef(using Context): TermRef = self match {
      case AndType(tp1, tp2) =>
        val c1 = tp1.classSymbol
        val c2 = tp2.classSymbol
        if c1.isSubClass(c2) then tp1.mirrorCompanionRef
        else tp2.mirrorCompanionRef // precondition: the parts of the AndType have already been checked to be non-overlapping
      case self @ TypeRef(prefix, _) if self.symbol.isClass =>
        prefix.select(self.symbol.companionModule).asInstanceOf[TermRef]
      case self: TypeProxy =>
        self.superType.mirrorCompanionRef
    }

    /** Is this type a methodic type that takes at least one parameter? */
    def takesParams(using Context): Boolean = self.stripPoly match
      case mt: MethodType => mt.paramNames.nonEmpty || mt.resType.takesParams
      case _ => false

    /** Is this type a methodic type that takes implicit parameters (both old and new) at some point? */
    def takesImplicitParams(using Context): Boolean = self.stripPoly match
      case mt: MethodType => mt.isImplicitMethod || mt.resType.takesImplicitParams
      case _ => false
  }
}
