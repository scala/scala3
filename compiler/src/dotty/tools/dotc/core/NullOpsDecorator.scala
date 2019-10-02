package dotty.tools.dotc.core

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Types.{AndType, ClassInfo, ConstantType, OrType, Type, TypeBounds, TypeMap, TypeProxy}

/** Defines operations on nullable types. */
object NullOpsDecorator {

  implicit class NullOps(val self: Type) {

    /** Is this type a reference to `Null`, possibly after aliasing? */
    def isNullType(implicit ctx: Context): Boolean = self.isRef(defn.NullClass)

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
     *  e.g. `JavaNull | (T1 | Null) | Null` => `T1 | Null | JavaNull`
     *
     *  1. If self is not a union type, the result is not a union type.
     *  2. If self is a union type,
     *    (1) If it only contains `Null` (or `JavaNull`) types, the result is still a union
              only contains `Null` types.
     *    (2) If it contains multiple Null types, at most 2 different Null types
     *        will appear at the right of result.
     *    (3) If it contains both `Null` and `JavaNull`, the result is `(tp | Null) | JavaNull`.
     *    (4) If it onlt contains `Null` or `JavaNull`, the result is `tp | Null` or `tp | JavaNull`.
     */
     def normNullableUnion(implicit ctx: Context): Type = {
      var isUnion = false
      var hasNull = false
      var hasJavaNull = false
      def strip(tp: Type): Type = tp match {
        case tp @ OrType(lhs, rhs) =>
          isUnion = true
          val llhs = strip(lhs)
          val rrhs = strip(rhs)
          if (rrhs.isNullType) llhs
          else if (llhs.isNullType) rrhs
          else tp.derivedOrType(llhs, rrhs)
        case _ =>
          if (tp.isNullType) {
            if (tp.isJavaNullType) hasJavaNull = true
            else hasNull = true
          }
          tp
      }
      val tp = strip(self)
      if (!isUnion) self
      else {
        val tp1 = if (hasNull) OrType(tp, defn.NullType) else tp
        if (hasJavaNull) OrType(tp1, defn.JavaNullAliasType) else tp1
      }
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
        // If it a `JavaNull` Union, the most right type must be a `JavaNull`.
        case OrType(_, rhs) => rhs.isJavaNullType
        case _ => false
      }
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

    /** Syntactically strips the nullability from this type.
     *  If the normalized form (as per `normNullableUnion`) of this type is `T1 | ... | Tn-1 | Tn`,
     *  and `Tn-1` `Tn` are references to `Null`, then return `T1 | ... | Tn-2`.
     *  If this type isn't (syntactically) nullable, then returns the type unchanged.
     */
    def stripNull(implicit ctx: Context): Type = {
      assert(ctx.explicitNulls)
      self.widenDealias.normNullableUnion match {
        case OrType(lhs, rhs) if rhs.isNullType =>
          lhs match {
            case OrType(llhs, lrhs) if lrhs.isNullType =>
              // The result type contains at most 2 `Null` (or aliases to `Null`) type.
              llhs
            case _ =>
              lhs
          }
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
      var diff = false
      object RemoveNulls extends TypeMap {
        override def apply(tp: Type): Type =
          tp.normNullableUnion match {
            // It it is JavaNullable Union
            case OrType(lhs, rhs) if rhs.isJavaNullType =>
              diff = true
              mapOver(lhs)
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
