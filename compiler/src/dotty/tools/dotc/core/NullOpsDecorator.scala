package dotty.tools.dotc.core

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Types.{AndType, ClassInfo, ConstantType, OrType, Type, TypeBounds, TypeMap, TypeProxy}

/** Defines operations on nullable types. */
object NullOpsDecorator {

  implicit class NullOps(val self: Type) {
    /** Is this type a reference to `Null`, possibly after aliasing? */
    def isNullType(implicit ctx: Context): Boolean = self.isRef(defn.NullClass)

    /** Is self (after widening and dealiasing) a type of the form `T | Null`? */
    def isNullableUnion(implicit ctx: Context): Boolean = {
      assert(ctx.explicitNulls)
      self.widenDealias.normNullableUnion match {
        case OrType(_, right) => right.isNullType
        case _ => false
      }
    }

    /** Is this type exactly `JavaNull` (no vars, aliases, refinements etc allowed)? */
    def isJavaNullType(implicit ctx: Context): Boolean = {
      assert(ctx.explicitNulls)
      // We can't do `self == defn.JavaNull` because when trees are unpickled new references
      // to `JavaNull` could be created that are different from `defn.JavaNull`.
      // Instead, we compare the symbol.
      self.isDirectRef(defn.JavaNullAlias)
    }

    /** Is self (after widening and dealiasing) a type of the form `T | JavaNull`? */
    def isJavaNullableUnion(implicit ctx: Context): Boolean = {
      assert(ctx.explicitNulls)
      def peelOff(tpe: Type): Boolean = tpe match {
        case OrType(left, right) =>
          right.isJavaNullType || right.isNullType && peelOff(left)
        case _ => false
      }
      val norm = self.widenDealias.normNullableUnion
      // We can't just look at the r.h.s of the normalized union.
      // The problem is that after normalizing we could get a type like `(String | JavaNull) | Null`,
      // where the r.h.s of the union isn't a `JavaNull`, but if we keep peeling nulls off starting
      // from the right we'll eventually get to the `JavaNull`.
      peelOff(norm)
    }

    /** Is this type guaranteed not to have `null` as a value? */
    final def isNotNull(implicit ctx: Context): Boolean = self match {
      case tp: ConstantType => tp.value.value != null
      case tp: ClassInfo => !tp.cls.isNullableClass && tp.cls != defn.NothingClass
      case tp: TypeBounds => tp.lo.isNotNull
      case tp: TypeProxy => tp.underlying.isNotNull
      case AndType(tp1, tp2) => tp1.isNotNull || tp2.isNotNull
      case OrType(tp1, tp2) => tp1.isNotNull && tp2.isNotNull
      case _ => false
    }

    def maybeNullable(implicit ctx: Context): Type =
      if (ctx.explicitNulls) OrType(self, defn.NullType) else self

    /** Normalizes unions so that all `Null`s (or aliases to `Null`) appear to the right of all other types.
     *  In the process, it also flattens the type so that there are no nested unions at the top level.
     *  e.g. `Null | (T1 | Null) | T2` => `T1 | T2 | Null | Null`
     */
    def normNullableUnion(implicit ctx: Context): Type = {
      assert(ctx.explicitNulls)
      def split(tp: Type, nonNull: List[Type], nll: List[Type]): (List[Type], List[Type]) = {
        tp match {
          case OrType(left, right) =>
            // Recurse on the right first so we get the types in pre-order.
            val (nonNull1, nll1) = split(right, nonNull, nll)
            split(left, nonNull1, nll1)
          case _ =>
            if (tp.isNullType) (nonNull, tp :: nll) else (tp :: nonNull, nll)
        }
      }
      val (nonNull, nll) = split(self, Nil, Nil)
      val all = nonNull ++ nll
      assert(all.nonEmpty)
      all.tail.foldLeft(all.head)(OrType.apply)
    }

    /** Strips `Null` from type unions in this type.
     *  @param onlyJavaNull if true, we delete only `JavaNull` and not vanilla `Null`.
     */
    private def stripNullImpl(onlyJavaNull: Boolean)(implicit ctx: Context): Type = {
      assert(ctx.explicitNulls)
      def strip(tp: Type, changed: Boolean): (Type, Boolean) = {
        tp match {
          case OrType(left, right) if right.isNullType =>
            if (!onlyJavaNull || right.isJavaNullType) strip(left, changed = true)
            else {
              val (tp1, changed1) = strip(left, changed)
              (OrType(tp1, right), changed1)
            }
          case _ => (tp, changed)
        }
      }
      // If there are no `Null`s to strip off, try to keep the method a no-op
      // by keeping track of whether the result has changed.
      // Otherwise, we would widen and dealias as a side effect.
      val (tp, diff) = strip(self.widenDealias.normNullableUnion, changed = false)
      if (diff) tp else self
    }

    /** Syntactically strips the nullability from this type. If the normalized form (as per `normNullableUnion`)
     *  of this type is `T1 | ... | Tk | ... | Tn`, and all types in the range `Tk ... Tn` are references to `Null`,
     *  then return `T1 | ... | Tk-1`.
     *  If this type isn't (syntactically) nullable, then returns the type unchanged.
     */
    def stripNull(implicit ctx: Context): Type = {
      assert(ctx.explicitNulls)
      stripNullImpl(onlyJavaNull = false)
    }

    /** Like `stripNull`, but removes only the `JavaNull`s. */
    def stripJavaNull(implicit ctx: Context): Type = {
      assert(ctx.explicitNulls)
      stripNullImpl(onlyJavaNull = true)
    }

    /** Collapses all `JavaNull` unions within this type, and not just the outermost ones (as `stripJavaNull` does).
     *  e.g. (Array[String|Null]|Null).stripNull => Array[String|Null]
     *       (Array[String|Null]|Null).stripInnerNulls => Array[String]
     *  If no `JavaNull` unions are found within the type, then returns the input type unchanged.
     */
    def stripAllJavaNull(implicit ctx: Context): Type = {
      assert(ctx.explicitNulls)
      var diff = false
      object RemoveNulls extends TypeMap {
        override def apply(tp: Type): Type = tp match {
          case tp: OrType if tp.isJavaNullableUnion =>
            diff = true
            mapOver(tp.stripJavaNull)
          case _ => mapOver(tp)
        }
      }
      val rem = RemoveNulls(self.widenDealias)
      if (diff) rem else self
    }

    /** Injects this type into a union with `JavaNull`. */
    def toJavaNullableUnion(implicit ctx: Context): Type = {
      assert(ctx.explicitNulls)
      if (self.isJavaNullableUnion) self
      else OrType(self, defn.JavaNullAliasType)
    }
  }
}
