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

    /** Syntactically strips the nullability from this type.
     *  If the normalized form (as per `normNullableUnion`) of this type is `T1 | ... | Tn-1 | Tn`,
     *  and `Tn` references to `Null` (or `JavaNull`), then return `T1 | ... | Tn-1`.
     *  If this type isn't (syntactically) nullable, then returns the type unchanged.
     *
     *  @param onlyJavaNull whether we only remove `JavaNull`, the default value is false
     */
    def stripNull(onlyJavaNull: Boolean = false)(implicit ctx: Context): Type = {
      assert(ctx.explicitNulls)

      def isNull(tp: Type) =
        if (onlyJavaNull) tp.isJavaNullType
        else tp.isNullType

      def strip(tp: Type): Type = tp match {
        case tp @ OrType(lhs, rhs) =>
          val llhs = strip(lhs)
          val rrhs = strip(rhs)
          if (isNull(rrhs)) llhs
          else if (isNull(llhs)) rrhs
          else tp.derivedOrType(llhs, rrhs)
        case tp @ AndType(tp1, tp2) =>
          // We cannot `tp.derivedAndType(strip(tp1), strip(tp2))` directly,
          // since `normNullableUnion((A | Null) & B)` would produce the wrong
          // result `(A & B) | Null`.
          val tp1s = strip(tp1)
          val tp2s = strip(tp2)
          if((tp1s ne tp1) && (tp2s ne tp2))
            tp.derivedAndType(tp1s, tp2s)
          else tp
        case _ => tp
      }

      val self1 = self.widenDealias
      val striped = strip(self1)
      if (striped ne self1) striped else self
    }

    /** Like `stripNull`, but removes only the `JavaNull`s. */
    def stripJavaNull(implicit ctx: Context): Type = self.stripNull(true)

    /** Collapses all `JavaNull` unions within this type, and not just the outermost ones (as `stripJavaNull` does).
     *  e.g. (Array[String|Null]|Null).stripNull => Array[String|Null]
     *       (Array[String|Null]|Null).stripInnerNulls => Array[String]
     *  If no `JavaNull` unions are found within the type, then returns the input type unchanged.
     */
    def stripAllJavaNull(implicit ctx: Context): Type = {
      object RemoveNulls extends TypeMap {
        override def apply(tp: Type): Type = mapOver(tp.stripNull(true))
      }
      val rem = RemoveNulls(self)
      if (rem ne self) rem else self
    }

    /** Is self (after widening and dealiasing) a type of the form `T | Null`? */
    def isNullableUnion(implicit ctx: Context): Boolean = {
      val striped = self.stripNull()
      striped ne self
    }

    /** Is self (after widening and dealiasing) a type of the form `T | JavaNull`? */
    def isJavaNullableUnion(implicit ctx: Context): Boolean = {
      val striped = self.stripNull(true)
      striped ne self
    }
  }
}
