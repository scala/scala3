package dotty.tools
package dotc
package typer

import core._
import ast._
import Contexts._, Types._, Flags._, Denotations._, Names._, StdNames._, NameOps._, Symbols._
import Trees._
import Constants._
import Scopes._
import ProtoTypes._
import annotation.unchecked
import util.Positions._
import util.{Stats, SimpleMap}
import util.common._
import Decorators._
import Uniques._
import ErrorReporting.{errorType, DiagnosticString}
import config.Printers._
import collection.mutable

trait Inferencing { this: Checking =>

  import tpd._

  /** Is type fully defined, meaning the type does not contain wildcard types
   *  or uninstantiated type variables. As a side effect, this will minimize
   *  any uninstantiated type variables, according to the given force degree,
   *  but only if the overall result of `isFullyDefined` is `true`.
   *  Variables that are successfully minimized do not count as uninstantiated.
   */
  def isFullyDefined(tp: Type, force: ForceDegree.Value)(implicit ctx: Context): Boolean = {
    val nestedCtx = ctx.fresh.setNewTyperState
    val result = new IsFullyDefinedAccumulator(force)(nestedCtx).process(tp)
    if (result) nestedCtx.typerState.commit()
    result
  }

  /** The fully defined type, where all type variables are forced.
   *  Throws an error if type contains wildcards.
   */
  def fullyDefinedType(tp: Type, what: String, pos: Position)(implicit ctx: Context) =
    if (isFullyDefined(tp, ForceDegree.all)) tp
    else throw new Error(i"internal error: type of $what $tp is not fully defined, pos = $pos") // !!! DEBUG

  /** The accumulator which forces type variables using the policy encoded in `force`
   *  and returns whether the type is fully defined. Two phases:
   *  1st Phase: Try to stantiate covariant and non-variant type variables to
   *  their lower bound. Record whether succesful.
   *  2nd Phase: If first phase was succesful, instantiate all remaining type variables
   *  to their upper bound.
   */
  private class IsFullyDefinedAccumulator(force: ForceDegree.Value)(implicit ctx: Context) extends TypeAccumulator[Boolean] {
    private def instantiate(tvar: TypeVar, fromBelow: Boolean): Type = {
      val inst = tvar.instantiate(fromBelow)
      typr.println(i"forced instantiation of ${tvar.origin} = $inst")
      inst
    }
    private var toMaximize: Boolean = false
    def apply(x: Boolean, tp: Type): Boolean = tp.dealias match {
      case _: WildcardType | _: ProtoType =>
        false
      case tvar: TypeVar if !tvar.isInstantiated =>
        if (force == ForceDegree.none) false
        else {
          val minimize =
            variance >= 0 && !(
              force == ForceDegree.noBottom &&
              isBottomType(ctx.typeComparer.approximation(tvar.origin, fromBelow = true)))
          if (minimize) instantiate(tvar, fromBelow = true)
          else toMaximize = true
          foldOver(x, tvar)
        }
      case tp =>
        foldOver(x, tp)
    }

    private class UpperInstantiator(implicit ctx: Context) extends TypeAccumulator[Unit] {
      def apply(x: Unit, tp: Type): Unit = {
        tp match {
          case tvar: TypeVar if !tvar.isInstantiated =>
            instantiate(tvar, fromBelow = false)
          case _ =>
        }
        foldOver(x, tp)
      }
    }

    def process(tp: Type): Boolean = {
      val res = apply(true, tp)
      if (res && toMaximize) new UpperInstantiator().apply((), tp)
      res
    }
  }

  def isBottomType(tp: Type)(implicit ctx: Context) =
    tp == defn.NothingType || tp == defn.NullType

  /** Recursively widen and also follow type declarations and type aliases. */
  def widenForMatchSelector(tp: Type)(implicit ctx: Context): Type = tp.widen match {
    case tp: TypeRef if !tp.symbol.isClass => widenForMatchSelector(tp.info.bounds.hi)
    case tp: AnnotatedType => tp.derivedAnnotatedType(tp.annot, widenForMatchSelector(tp.tpe))
    case tp => tp
  }

  /** Following type aliases and stripping refinements and annotations, if one arrives at a
   *  class type reference where the class has a companion module, a reference to
   *  that companion module. Otherwise NoType
   */
  def companionRef(tp: Type)(implicit ctx: Context): Type =
    tp.underlyingClassRef(refinementOK = true) match {
      case tp: TypeRef =>
        val companion = tp.classSymbol.companionModule
        if (companion.exists)
          companion.valRef.asSeenFrom(tp.prefix, companion.symbol.owner)
        else NoType
      case _ => NoType
    }

  /** Ensure that the first type in a list of parent types Ps points to a non-trait class.
   *  If that's not already the case, add one. The added class type CT is determined as follows.
   *  First, let C be the unique class such that
   *  - there is a parent P_i such that P_i derives from C, and
   *  - for every class D: If some parent P_j, j <= i derives from D, then C derives from D.
   *  Then, let CT be the smallest type which
   *  - has C as its class symbol, and
   *  - for all parents P_i: If P_i derives from C then P_i <:< CT.
   */
  def ensureFirstIsClass(parents: List[Type])(implicit ctx: Context): List[Type] = {
    def realClassParent(cls: Symbol): ClassSymbol =
      if (!cls.isClass) defn.ObjectClass
      else if (!(cls is Trait)) cls.asClass
      else cls.asClass.classParents match {
        case parentRef :: _ => realClassParent(parentRef.symbol)
        case nil => defn.ObjectClass
      }
    def improve(candidate: ClassSymbol, parent: Type): ClassSymbol = {
      val pcls = realClassParent(parent.classSymbol)
      if (pcls derivesFrom candidate) pcls else candidate
    }
    parents match {
      case p :: _ if p.classSymbol.isRealClass => parents
      case _ =>
        val pcls = (defn.ObjectClass /: parents)(improve)
        typr.println(i"ensure first is class $parents%, % --> ${parents map (_ baseTypeWithArgs pcls)}%, %")
        val ptype = ctx.typeComparer.glb(
            defn.ObjectType :: (parents map (_ baseTypeWithArgs pcls)))
        ptype :: parents
    }
  }

  /** Ensure that first parent tree refers to a real class. */
  def ensureFirstIsClass(parents: List[Tree], pos: Position)(implicit ctx: Context): List[Tree] = parents match {
    case p :: ps if p.tpe.classSymbol.isRealClass => parents
    case _ =>
      // add synthetic class type
      val first :: _ = ensureFirstIsClass(parents.tpes)
      TypeTree(checkFeasible(first, pos, d"\n in inferred parent $first")).withPos(pos) :: parents
  }

  /** Interpolate those undetermined type variables in the widened type of this tree
   *  which are introduced by type application contained in the tree.
   *  If such a variable appears covariantly in type `tp` or does not appear at all,
   *  approximate it by its lower bound. Otherwise, if it appears contravariantly
   *  in type `tp` approximate it by its upper bound.
   *  @param ownedBy  if it is different from NoSymbol, all type variables owned by
   *                  `ownedBy` qualify, independent of position.
   *                  Without that second condition, it can be that certain variables escape
   *                  interpolation, for instance when their tree was eta-lifted, so
   *                  the typechecked tree is no longer the tree in which the variable
   *                  was declared. A concrete example of this phenomenon can be
   *                  observed when compiling core.TypeOps#asSeenFrom.
   */
  def interpolateUndetVars(tree: Tree, ownedBy: Symbol)(implicit ctx: Context): Unit = {
    val constraint = ctx.typerState.constraint
    val qualifies = (tvar: TypeVar) =>
      (tree contains tvar.owningTree) || ownedBy.exists && tvar.owner == ownedBy
    def interpolate() = Stats.track("interpolateUndetVars") {
      val tp = tree.tpe.widen
      constr.println(s"interpolate undet vars in ${tp.show}, pos = ${tree.pos}, mode = ${ctx.mode}, undets = ${constraint.uninstVars map (tvar => s"${tvar.show}@${tvar.owningTree.pos}")}")
      constr.println(s"qualifying undet vars: ${constraint.uninstVars filter qualifies map (tvar => s"$tvar / ${tvar.show}")}, constraint: ${constraint.show}")

      val vs = variances(tp, qualifies)
      var changed = false
      vs foreachBinding { (tvar, v) =>
        if (v != 0) {
          typr.println(s"interpolate ${if (v == 1) "co" else "contra"}variant ${tvar.show} in ${tp.show}")
          tvar.instantiate(fromBelow = v == 1)
          changed = true
        }
      }
      if (changed) // instantiations might have uncovered new typevars to interpolate
        interpolateUndetVars(tree, ownedBy)
      else
        for (tvar <- constraint.uninstVars)
          if (!(vs contains tvar) && qualifies(tvar)) {
            typr.println(s"instantiating non-occurring ${tvar.show} in ${tp.show}")
            tvar.instantiate(fromBelow = true)
          }
    }
    if (constraint.uninstVars exists qualifies) interpolate()
  }

  /** Instantiate undetermined type variables to that type `tp` is
   *  maximized and return None. If this is not possible, because a non-variant
   *  typevar is not uniquely determined, return that typevar in a Some.
   */
  def maximizeType(tp: Type)(implicit ctx: Context): Option[TypeVar] = Stats.track("maximizeType") {
    val vs = variances(tp, alwaysTrue)
    var result: Option[TypeVar] = None
    vs foreachBinding { (tvar, v) =>
      if (v == 1) tvar.instantiate(fromBelow = false)
      else if (v == -1) tvar.instantiate(fromBelow = true)
      else {
        val bounds = ctx.typerState.constraint.fullBounds(tvar.origin)
        if (!(bounds.hi <:< bounds.lo)) result = Some(tvar)
        tvar.instantiate(fromBelow = false)
      }
    }
    result
  }

  type VarianceMap = SimpleMap[TypeVar, Integer]

  /** All occurrences of type vars in this type that satisfy predicate
   *  `include` mapped to their variances (-1/0/1) in this type, where
   *  -1 means: only covariant occurrences
   *  +1 means: only covariant occurrences
   *  0 means: mixed or non-variant occurrences
   *
   *  Note: We intentionally use a relaxed version of variance here,
   *  where the variance does not change under a prefix of a named type
   *  (the strict version makes prefixes invariant). This turns out to be
   *  better for type inference. In a nutshell, if a type variable occurs
   *  like this:
   *
   *     (U? >: x.type) # T
   *
   *  we want to instantiate U to x.type right away. No need to wait further.
   */
  private def variances(tp: Type, include: TypeVar => Boolean)(implicit ctx: Context): VarianceMap = Stats.track("variances") {
    val accu = new TypeAccumulator[VarianceMap] {
      def apply(vmap: VarianceMap, t: Type): VarianceMap = t match {
        case t: TypeVar if !t.isInstantiated && (ctx.typerState.constraint contains t) && include(t) =>
          val v = vmap(t)
          if (v == null) vmap.updated(t, variance)
          else if (v == variance) vmap
          else vmap.updated(t, 0)
        case _ =>
          foldOver(vmap, t)
      }
      override def applyToPrefix(vmap: VarianceMap, t: NamedType) =
        apply(vmap, t.prefix)
    }
    accu(SimpleMap.Empty, tp)
  }
}

/** An enumeration controlling the degree of forcing in "is-dully-defined" checks. */
object ForceDegree extends Enumeration {
  val none,           // don't force type variables
      noBottom,       // force type variables, fail if forced to Nothing or Null
      all = Value     // force type variables, don't fail
}

