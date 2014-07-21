package dotty.tools.dotc
package transform

import dotty.tools.dotc.ast.{ Trees, tpd }
import core._
import Types._, Contexts._, Flags._, Symbols._, Annotations._, Trees._
import Decorators._
import Variances._

object VarianceChecker {

  case class VarianceError(tvar: Symbol, required: Variance)
}

/** See comments at scala.reflect.internal.Variance.
 */
class VarianceChecker(implicit ctx: Context) {
  import VarianceChecker._
  import tpd._

  private object Validator extends TypeAccumulator[Option[VarianceError]] {
    private var base: Symbol = _

    /** The variance of a symbol occurrence of `tvar` seen at the level of the definition of `base`.
     *  The search proceeds from `base` to the owner of `tvar`.
     *  Initially the state is covariant, but it might change along the search.
     */
    def relativeVariance(tvar: Symbol, base: Symbol, v: Variance = Covariant): Variance = {
      if (base.owner == tvar.owner) v
      else if ((base is Param) && base.owner.isTerm) relativeVariance(tvar, base.owner.owner, flip(v))
      else if (base.isTerm) Bivariant
      else if (base.isAliasType) relativeVariance(tvar, base.owner, Invariant)
      else relativeVariance(tvar, base.owner, v)
    }

    def isUncheckedVariance(tp: Type): Boolean = tp match {
      case AnnotatedType(annot, tp1) =>
        annot.symbol == defn.UncheckedVarianceAnnot || isUncheckedVariance(tp1)
      case _ => false
    }

    private def checkVarianceOfSymbol(tvar: Symbol): Option[VarianceError] = {
      val relative = relativeVariance(tvar, base)
      val required = Variances.compose(relative, this.variance)
      if (relative == Bivariant) None
      else {
        def tvar_s = s"$tvar (${tvar.variance}${tvar.showLocated})"
        def base_s = s"$base in ${base.owner}" + (if (base.owner.isClass) "" else " in " + base.owner.enclosingClass)
        ctx.log(s"verifying $tvar_s is $required at $base_s")
        if (tvar.variance == required) None
        else Some(VarianceError(tvar, required))
      }
    }

    /** For PolyTypes, type parameters are skipped because they are defined
     *  explicitly (their TypeDefs will be passed here.) For MethodTypes, the
     *  same is true of the parameters (ValDefs) unless we are inside a
     *  refinement, in which case they are checked from here.
     */
    def apply(status: Option[VarianceError], tp: Type): Option[VarianceError] =
      if (status.isDefined) status
      else tp match {
        case tp: TypeRef =>
          val sym = tp.symbol
          if (sym.variance != 0 && base.isContainedIn(sym.owner)) checkVarianceOfSymbol(sym)
          else if (sym.isAliasType) this(status, sym.info)
          else foldOver(status, tp)
        case tp: MethodType =>
          this(status, tp.resultType) // params will be checked in their TypeDef nodes.
        case tp: PolyType =>
          this(status, tp.resultType) // params will be checked in their ValDef nodes.
        case AnnotatedType(annot, _) if annot.symbol == defn.UncheckedVarianceAnnot =>
          status
        case tp: ClassInfo =>
          ???
        case _ =>
          foldOver(status, tp)
      }

    def validateDefinition(base: Symbol): Option[VarianceError] = {
      val saved = this.base
      this.base = base
      try apply(None, base.info)
      finally this.base = saved
    }
  }

  def varianceString(v: Variance) =
    if (v is Covariant) "covariant"
    else if (v is Contravariant) "contravariant"
    else "invariant"

  object Traverser extends TreeTraverser {
    def checkVariance(sym: Symbol) = Validator.validateDefinition(sym) match {
      case Some(VarianceError(tvar, required)) =>
        ctx.error(
          i"${varianceString(tvar.flags)} $tvar occurs in ${varianceString(required)} position in type ${sym.info} of $sym",
          sym.pos)
      case None =>
    }

    override def traverse(tree: Tree) = {
      def sym = tree.symbol
      // No variance check for object-private/protected methods/values.
      // Or constructors, or case class factory or extractor.
      def skip = (
           sym == NoSymbol
        || sym.is(Local)
        || sym.owner.isConstructor
        //|| sym.owner.isCaseApplyOrUnapply // not clear why needed
      )
      tree match {
        case defn: MemberDef if skip =>
          ctx.debuglog(s"Skipping variance check of ${sym.showDcl}")
        case tree: TypeDef =>
          checkVariance(sym)
          super.traverse(tree)
        case tree: ValDef =>
          checkVariance(sym)
        case DefDef(_, _, tparams, vparamss, _, _) =>
          checkVariance(sym)
          tparams foreach traverse
          vparamss foreach (_ foreach traverse)
        case Template(_, _, _, body) =>
          super.traverse(tree)
        case _ =>
      }
    }
  }
}
