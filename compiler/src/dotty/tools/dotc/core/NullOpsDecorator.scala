package dotty.tools.dotc.core

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Types.{AndType, ClassInfo, ConstantType, OrType, Type, TypeBounds, TypeMap, TypeProxy}

/** Defines operations on nullable types. */
object NullOpsDecorator {

  implicit class NullOps(val self: Type) {
    /** Is this type exactly `JavaNull` (no vars, aliases, refinements etc allowed)? */
    def isJavaNullType(implicit ctx: Context): Boolean = {
      assert(ctx.explicitNulls)
      // We can't do `self == defn.JavaNull` because when trees are unpickled new references
      // to `JavaNull` could be created that are different from `defn.JavaNull`.
      // Instead, we compare the symbol.
      self.isDirectRef(defn.JavaNullAlias)
    }

    /** Normalizes unions so that all `Null`s (or aliases to `Null`) appear to the right of
     *  all other types.
     *  e.g. `Null | (T1 | Null) | T2` => `T1 | T2 | Null`
     *  e.g. `JavaNull | (T1 | Null) | Null` => `T1 | JavaNull`
     *
     *  Let `self` denote the current type:
     *  1. If `self` is not a union, then the result is not a union and equal to `self`.
     *  2. If `self` is a union then
     *    2.1 If `self` does not contain `Null` as part of the union, then the result is `self`.
     *    2.2 If `self` contains `Null` (resp `JavaNull`) as part of the union, let `self2` denote
     *      the same type as `self`, but where all instances of `Null` (`JavaNull`) in the union
     *      have been removed. Then the result is `self2 | Null` (`self2 | JavaNull`).
     */
     def normNullableUnion(implicit ctx: Context): Type = {
      var hasNull = false
      var hasJavaNull = false
      def strip(tp: Type): Type = tp match {
        case tp @ OrType(lhs, rhs) =>
          val llhs = strip(lhs)
          val rrhs = strip(rhs)
          if (rrhs.isNullType) llhs
          else if (llhs.isNullType) rrhs
          else tp.derivedOrType(llhs, rrhs)
        case tp @ AndType(tp1, tp2) =>
          // We cannot `tp.derivedAndType(strip(tp1), strip(tp2))` directly,
          // since `normNullableUnion((A | Null) & B)` would produce the wrong
          // result `(A & B) | Null`.
          val oldHN = hasNull
          val oldHJN = hasJavaNull
          val tp1s = strip(tp1)
          val tp2s = strip(tp2)
          if((tp1s ne tp1) && (tp2s ne tp2))
            tp.derivedAndType(tp1s, tp2s)
          else
            // If tp1 or tp2 is not nullable, we should revert the change of
            // `hasNull` and `hasJavaNull` and return the original tp.
            hasNull = oldHN
            hasJavaNull = oldHJN
            tp
        case _ =>
          if (tp.isNullType) {
            if (tp.isJavaNullType) hasJavaNull = true
            else hasNull = true
          }
          tp
      }
      val tp = strip(self)
      if (tp eq self) self
      else if (hasJavaNull) OrType(tp, defn.JavaNullAliasType)
      else if (hasNull) OrType(tp, defn.NullType)
      else self
    }

    /** Is self (after widening and dealiasing) a type of the form `T | Null`? */
    def isNullableUnion(implicit ctx: Context): Boolean = {
      assert(ctx.explicitNulls)
      self.widenDealias.normNullableUnion match {
        case OrType(_, rhs) => rhs.isNullType
        case _ => false
      }
    }

    /** Is self (after widening and dealiasing) a type of the form `T | JavaNull`? */
    def isJavaNullableUnion(implicit ctx: Context): Boolean = {
      assert(ctx.explicitNulls)
      self.widenDealias.normNullableUnion match {
        case OrType(_, rhs) => rhs.isJavaNullType
        case _ => false
      }
    }

    def maybeNullable(implicit ctx: Context): Type =
      if (ctx.explicitNulls) OrType(self, defn.NullType) else self

    /** Syntactically strips the nullability from this type.
     *  If the normalized form (as per `normNullableUnion`) of this type is `T1 | ... | Tn-1 | Tn`,
     *  and `Tn` references to `Null` (or `JavaNull`), then return `T1 | ... | Tn-1`.
     *  If this type isn't (syntactically) nullable, then returns the type unchanged.
     */
    def stripNull(implicit ctx: Context): Type = {
      assert(ctx.explicitNulls)
      self.widenDealias.normNullableUnion match {
        case OrType(lhs, rhs) if rhs.isNullType => lhs
        case _ => self
      }
    }

    /** Like `stripNull`, but removes only the `JavaNull`s. */
    def stripJavaNull(implicit ctx: Context): Type = {
      assert(ctx.explicitNulls)
      self.widenDealias.normNullableUnion match {
        case OrType(lhs, rhs) if rhs.isJavaNullType => lhs
        case _ => self
      }
    }

    /** Collapses all `JavaNull` unions within this type, and not just the outermost ones (as `stripJavaNull` does).
     *  e.g. (Array[String|Null]|Null).stripNull => Array[String|Null]
     *       (Array[String|Null]|Null).stripInnerNulls => Array[String]
     *  If no `JavaNull` unions are found within the type, then returns the input type unchanged.
     */
    def stripAllJavaNull(implicit ctx: Context): Type = {
      assert(ctx.explicitNulls)
      object RemoveNulls extends TypeMap {
        override def apply(tp: Type): Type =
          tp.normNullableUnion match {
            case OrType(lhs, rhs) if rhs.isJavaNullType =>
              mapOver(lhs)
            case _ => mapOver(tp)
          }
      }
      val self1 = self.widenDealias
      val rem = RemoveNulls(self1)
      if (rem ne self1) rem else self
    }

    /** Injects this type into a union with `JavaNull`. */
    def toJavaNullableUnion(implicit ctx: Context): Type = {
      assert(ctx.explicitNulls)
      if (self.isJavaNullableUnion) self
      else OrType(self, defn.JavaNullAliasType)
    }
  }
}
