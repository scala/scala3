package dotty.tools.dotc.core

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Types._

/** Defines operations on nullable types. */
object NullOpsDecorator {

  implicit class NullOps(val self: Type) {
    /** Is this type exactly `UncheckedNull` (no vars, aliases, refinements etc allowed)? */
    def isUncheckedNullType(implicit ctx: Context): Boolean = {
      assert(ctx.explicitNulls)
      // We can't do `self == defn.UncheckedNull` because when trees are unpickled new references
      // to `UncheckedNull` could be created that are different from `defn.UncheckedNull`.
      // Instead, we compare the symbol.
      self.isDirectRef(defn.UncheckedNullAlias)
    }

    /** Syntactically strips the nullability from this type.
     *  If the type is `T1 | ... | Tn`, and `Ti` references to `Null` (or `UncheckedNull`),
     *  then return `T1 | ... | Ti-1 | Ti+1 | ... | Tn`.
     *  If this type isn't (syntactically) nullable, then returns the type unchanged.
     *
     *  @param onlyUncheckedNull whether we only remove `UncheckedNull`, the default value is false
     */
    def stripNull(onlyUncheckedNull: Boolean = false)(implicit ctx: Context): Type = {
      assert(ctx.explicitNulls)

      def isNull(tp: Type) =
        if (onlyUncheckedNull) tp.isUncheckedNullType
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
          // since `stripNull((A | Null) & B)` would produce the wrong
          // result `(A & B) | Null`.
          val tp1s = strip(tp1)
          val tp2s = strip(tp2)
          if((tp1s ne tp1) && (tp2s ne tp2))
            tp.derivedAndType(tp1s, tp2s)
          else tp
        case _ => tp
      }

      val self1 = self.widenDealias
      val stripped = strip(self1)
      if (stripped ne self1) stripped else self
    }

    /** Like `stripNull`, but removes only the `UncheckedNull`s. */
    def stripUncheckedNull(implicit ctx: Context): Type = self.stripNull(true)

    /** Collapses all `UncheckedNull` unions within this type, and not just the outermost ones (as `stripUncheckedNull` does).
     *  e.g. (Array[String|UncheckedNull]|UncheckedNull).stripUncheckedNull => Array[String|UncheckedNull]
     *       (Array[String|UncheckedNull]|UncheckedNull).stripAllUncheckedNull => Array[String]
     *  If no `UncheckedNull` unions are found within the type, then returns the input type unchanged.
     */
    def stripAllUncheckedNull(implicit ctx: Context): Type = {
      object RemoveNulls extends TypeMap {
        override def apply(tp: Type): Type = mapOver(tp.stripNull(true))
      }
      val rem = RemoveNulls(self)
      if (rem ne self) rem else self
    }

    /** Is self (after widening and dealiasing) a type of the form `T | Null`? */
    def isNullableUnion(implicit ctx: Context): Boolean = {
      val stripped = self.stripNull()
      stripped ne self
    }

    /** Is self (after widening and dealiasing) a type of the form `T | UncheckedNull`? */
    def isUncheckedNullableUnion(implicit ctx: Context): Boolean = {
      val stripped = self.stripNull(true)
      stripped ne self
    }
  }
}
