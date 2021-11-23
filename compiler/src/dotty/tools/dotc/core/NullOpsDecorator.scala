package dotty.tools.dotc
package core

import ast.Trees._
import Contexts._
import Symbols.defn
import Types._

/** Defines operations on nullable types and tree. */
object NullOpsDecorator:

  private class StripNullsMap(isDeep: Boolean)(using Context) extends TypeMap:
    def strip(tp: Type): Type = tp match
      case tp @ OrType(lhs, rhs) =>
        val llhs = this(lhs)
        val rrhs = this(rhs)
        if rrhs.isNullType then llhs
        else if llhs.isNullType then rrhs
        else derivedOrType(tp, llhs, rrhs)
      case tp @ AndType(tp1, tp2) =>
        // We cannot `tp.derivedAndType(strip(tp1), strip(tp2))` directly,
        // since `stripNull((A | Null) & B)` would produce the wrong
        // result `(A & B) | Null`.
        val tp1s = this(tp1)
        val tp2s = this(tp2)
        if isDeep || (tp1s ne tp1) && (tp2s ne tp2) then
          derivedAndType(tp, tp1s, tp2s)
        else tp
      case tp: TypeBounds =>
        mapOver(tp)
      case _ => tp

    def stripOver(tp: Type): Type = tp match
      case appTp @ AppliedType(tycon, targs) =>
        derivedAppliedType(appTp, tycon, targs.map(this))
      case ptp: PolyType =>
        derivedLambdaType(ptp)(ptp.paramInfos, this(ptp.resType))
      case mtp: MethodType =>
        mapOver(mtp)
      case _ => strip(tp)

    override def apply(tp: Type): Type =
      if isDeep then stripOver(tp) else strip(tp)

  end StripNullsMap

  extension (self: Type)
    /** Syntactically strips the nullability from this type.
     *  If the type is `T1 | ... | Tn`, and `Ti` references to `Null`,
     *  then return `T1 | ... | Ti-1 | Ti+1 | ... | Tn`.
     *  If this type isn't (syntactically) nullable, then returns the type unchanged.
     *  The type will not be changed if explicit-nulls is not enabled.
     */
    def stripNull(using Context): Type = {
      if ctx.explicitNulls then
        val selfw = self.widenDealias
        val selfws = new StripNullsMap(false)(selfw)
        if selfws ne selfw then selfws else self
      else self
    }

    /** Is self (after widening and dealiasing) a type of the form `T | Null`? */
    def isNullableUnion(using Context): Boolean = {
      val stripped = self.stripNull
      stripped ne self
    }

    /** Strips nulls from this type deeply.
     *  Compaired to `stripNull`, `stripNullsDeep` will apply `stripNull` to
     *  each member of function types as well.
     */
    def stripNullsDeep(using Context): Type =
      if ctx.explicitNulls then
        val selfw = self.widenDealias
        val selfws = new StripNullsMap(true)(selfw)
        if selfws ne selfw then selfws else self
      else self

  end extension

  import ast.tpd._

  extension (self: Tree)
    // cast the type of the tree to a non-nullable type
    def castToNonNullable(using Context): Tree = self.typeOpt match {
      case OrNull(tp) => self.cast(tp)
      case _ => self
    }
end NullOpsDecorator