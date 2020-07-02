package dotty.tools.dotc.core

import Contexts.{Context, ctx}
import Symbols.defn
import Types._

/** Defines operations on nullable types. */
object NullOpsDecorator {

  extension (self: Type) {

    /** Syntactically strips the nullability from this type.
     *  If the type is `T1 | ... | Tn`, and `Ti` references to `Null`,
     *  then return `T1 | ... | Ti-1 | Ti+1 | ... | Tn`.
     *  If this type isn't (syntactically) nullable, then returns the type unchanged.
     */
    def stripNull(using Context): Type = {
      def strip(tp: Type): Type =
        val tpWiden = tp.widenDealias
        val tpStriped = tpWiden match {
          case tp @ OrType(lhs, rhs) =>
            val llhs = strip(lhs)
            val rrhs = strip(rhs)
            if rrhs.isNullType then llhs
            else if llhs.isNullType then rrhs
            else tp.derivedOrType(llhs, rrhs)
          case tp @ AndType(tp1, tp2) =>
            // We cannot `tp.derivedAndType(strip(tp1), strip(tp2))` directly,
            // since `stripNull((A | Null) & B)` would produce the wrong
            // result `(A & B) | Null`.
            val tp1s = strip(tp1)
            val tp2s = strip(tp2)
            if (tp1s ne tp1) && (tp2s ne tp2) then
              tp.derivedAndType(tp1s, tp2s)
            else tp
          case tp @ TypeBounds(lo, hi) =>
            tp.derivedTypeBounds(strip(lo), strip(hi))
          case tp => tp
        }
        if tpStriped ne tpWiden then tpStriped else tp

      strip(self)
    }

    def stripAllNulls(using Context): Type = {
      object RemoveNulls extends TypeMap {
        override def apply(tp: Type): Type =
          mapOver(tp.widenTermRefExpr.stripNull)
      }
      val rem = RemoveNulls(self)
      if rem ne self then rem else self
    }

    /** Is self (after widening and dealiasing) a type of the form `T | Null`? */
    def isNullableUnion(using Context): Boolean = {
      val stripped = self.stripNull
      stripped ne self
    }

    /** Can the type have null value after erasure?
     */
    def hasNullAfterErasure(using Context): Boolean = {
      self match {
        case tp: ClassInfo => tp.cls.isNullableClassAfterErasure
        case tp: TypeProxy => tp.underlying.hasNullAfterErasure
        case OrType(lhs, rhs) =>
          lhs.hasNullAfterErasure || rhs.hasNullAfterErasure
        case _ =>
          self <:< defn.ObjectType
      }
    }

    /** Can we convert a tree with type `self` to type `pt` unsafely.
     */
    def isUnsafeConvertable(pt: Type)(using Context): Boolean =
      (self.isNullType && pt.hasNullAfterErasure) ||
      (self.stripAllNulls <:< pt.stripAllNulls)
  }
}
