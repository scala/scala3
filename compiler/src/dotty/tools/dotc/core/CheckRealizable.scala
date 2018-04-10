package dotty.tools
package dotc
package core

import Contexts._, Types._, Symbols._, Names._, Flags._, Scopes._
import SymDenotations._, Denotations.SingleDenotation
import util.Positions._
import Decorators._
import StdNames._
import Annotations._
import collection.mutable
import ast.tpd._

/** Realizability status */
object CheckRealizable {

  abstract class Realizability(val msg: String) {
    def andAlso(other: => Realizability) =
      if (this == Realizable) other else this
    def mapError(f: Realizability => Realizability) =
      if (this == Realizable) this else f(this)
  }

  object Realizable extends Realizability("")

  object NotConcrete extends Realizability(" is not a concrete type")

  object NotStable extends Realizability(" is not a stable reference")

  class NotFinal(sym: Symbol)(implicit ctx: Context)
  extends Realizability(i" refers to nonfinal $sym")

  class HasProblemBounds(name: Name, info: Type)(implicit ctx: Context)
  extends Realizability(i" has a member $name with possibly conflicting bounds ${info.bounds.lo} <: ... <: ${info.bounds.hi}")

  class HasProblemBaseArg(typ: Type, argBounds: TypeBounds)(implicit ctx: Context)
  extends Realizability(i" has a base type $typ with possibly conflicting parameter bounds ${argBounds.lo} <: ... <: ${argBounds.hi}")

  class HasProblemBase(base1: Type, base2: Type)(implicit ctx: Context)
  extends Realizability(i" has conflicting base types $base1 and $base2")

  class HasProblemField(fld: SingleDenotation, problem: Realizability)(implicit ctx: Context)
  extends Realizability(i" has a member $fld which is not a legal path\nsince ${fld.symbol.name}: ${fld.info}${problem.msg}")

  class ProblemInUnderlying(tp: Type, problem: Realizability)(implicit ctx: Context)
  extends Realizability(i"s underlying type ${tp}${problem.msg}") {
    assert(problem != Realizable)
  }

  def realizability(tp: Type)(implicit ctx: Context) =
    new CheckRealizable().realizability(tp)

  def boundsRealizability(tp: Type)(implicit ctx: Context) =
    new CheckRealizable().boundsRealizability(tp)

  private val LateInitialized = Lazy | Erased,
}

/** Compute realizability status */
class CheckRealizable(implicit ctx: Context) {
  import CheckRealizable._

  /** A set of all fields that have already been checked. Used
   *  to avoid infinite recursions when analyzing recursive types.
   */
  private val checkedFields: mutable.Set[Symbol] = mutable.LinkedHashSet[Symbol]()

  /** Is symbol's definitition a lazy or erased val?
   *  (note we exclude modules here, because their realizability is ensured separately)
   */
  private def isLateInitialized(sym: Symbol) = sym.is(LateInitialized, butNot = Module)

  /** The realizability status of given type `tp`. We can only select members from realizable types.
    * A type is realizable if it has non-null values. However, realizable types can
    * have non-realizable subtypes, so we must restrict overriding. */
  def realizability(tp: Type): Realizability = tp.dealias match {
    case tp: TermRef =>
      // Suppose tp is a.b.c.type, where c is declared with type T, then sym is c, tp.info is T and
      // and tp.prefix is a.b.
      val sym = tp.symbol
      // We know tp is realizable if either:
      // 1. the symbol is stable and the prefix is realizable (so that, say, it contains no vars):
      if (sym.is(Stable)) realizability(tp.prefix)
      else {
        // 2. if tp.info is a realizable singleton type. We check this last
        // for performance, in all cases where some unrelated check might have failed.
        def patchRealizability(r: Realizability) =
          r.mapError(if (tp.info.isStableRealizable) Realizable else _)
        val r =
          // Reject fields that are mutable, by-name, and similar.
          if (!sym.isStable)
            patchRealizability(NotStable)
            // 3. If the symbol isn't "lazy" and its prefix is realizable
          else if (!isLateInitialized(sym))
            // XXX: This is a bit fishy: we only cache that the symbol is
            // stable if it appears under a realizable prefix.
            // XXX: Add object DependsOnPrefix extends Realizability(""), but filter it out here.
            patchRealizability(realizability(tp.prefix))
          else if (!sym.isEffectivelyFinal)
            patchRealizability(new NotFinal(sym))
          else
            // 4. If the symbol is effectively final, and a lazy or erased val
            // and has a realizable type. We require finality because overrides
            // of realizable fields might not be realizable.

            // Since patchRealizability checks realizability(tp.info) through
            // isStableRealizable, using patchRealizability wouldn't make
            // a difference, and calling it here again might introduce
            // a slowdown exponential in the prefix length.
            realizability(tp.info).mapError(r => new ProblemInUnderlying(tp.info, r))
        if (r == Realizable) sym.setFlag(Stable)
        r
      }
    case _: SingletonType | NoPrefix =>
      Realizable
    case tp =>
      def isConcrete(tp: Type): Boolean = tp.dealias match {
        case tp: TypeRef => tp.symbol.isClass
        case tp: TypeProxy => isConcrete(tp.underlying)
        case tp: AndType => isConcrete(tp.tp1) && isConcrete(tp.tp2)
        case tp: OrType  => isConcrete(tp.tp1) && isConcrete(tp.tp2)
        case _ => false
      }
      if (!isConcrete(tp)) NotConcrete
      else boundsRealizability(tp).andAlso(memberRealizability(tp))
  }

  private def refinedNames(tp: Type): Set[Name] = tp.dealias match {
    case tp: RefinedType => refinedNames(tp.parent) + tp.refinedName
    case tp: AndType => refinedNames(tp.tp1) ++ refinedNames(tp.tp2)
    case tp: OrType  => refinedNames(tp.tp1) ++ refinedNames(tp.tp2)
    case tp: TypeProxy => refinedNames(tp.underlying)
    case _ => Set.empty
  }

  /** `Realizable` if `tp` has good bounds, a `HasProblem...` instance
   *  pointing to a bad bounds member otherwise. "Has good bounds" means:
   *
   *    - all type members have good bounds
   *    - all base types are class types, and if their arguments are wildcards
   *      they have good bounds.
   *    - base types do not appear in multiple instances with different arguments.
   *      (depending on the simplification scheme for AndTypes employed, this could
   *       also lead to base types with bad bounds).
   */
  private def boundsRealizability(tp: Type) = {

    val memberProblems =
      for {
        mbr <- tp.nonClassTypeMembers
        if !(mbr.info.loBound <:< mbr.info.hiBound)
      }
      yield new HasProblemBounds(mbr.name, mbr.info)

    val refinementProblems =
      for {
        name <- refinedNames(tp)
        if (name.isTypeName)
        mbr <- tp.member(name).alternatives
        if !(mbr.info.loBound <:< mbr.info.hiBound)
      }
      yield new HasProblemBounds(name, mbr.info)

    def baseTypeProblems(base: Type) = base match {
      case AndType(base1, base2) =>
        new HasProblemBase(base1, base2) :: Nil
      case base =>
        base.argInfos.collect {
          case bounds @ TypeBounds(lo, hi) if !(lo <:< hi) =>
            new HasProblemBaseArg(base, bounds)
        }
    }
    val baseProblems =
      tp.baseClasses.map(_.baseTypeOf(tp)).flatMap(baseTypeProblems)

    ((((Realizable: Realizability)
      /: memberProblems)(_ andAlso _)
      /: refinementProblems)(_ andAlso _)
      /: baseProblems)(_ andAlso _)
  }

  /** `Realizable` if all of `tp`'s non-strict fields have realizable types,
   *  a `HasProblemField` instance pointing to a bad field otherwise.
   */
  private def memberRealizability(tp: Type) = {
    def checkField(sofar: Realizability, fld: SingleDenotation): Realizability =
      sofar andAlso {
        if (checkedFields.contains(fld.symbol) || fld.symbol.is(Private | Mutable | LateInitialized))
          // if field is private it cannot be part of a visible path
          // if field is mutable it cannot be part of a path
          // if field is lazy or erased it does not need to be initialized when the owning object is
          // so in all cases the field does not influence realizability of the enclosing object.
          Realizable
        else {
          checkedFields += fld.symbol
          realizability(fld.info).mapError(r => new HasProblemField(fld, r))
        }
      }
    if (ctx.settings.strict.value)
      // check fields only under strict mode for now.
      // Reason: An embedded field could well be nullable, which means it
      // should not be part of a path and need not be checked; but we cannot recognize
      // this situation until we have a typesystem that tracks nullability.
      ((Realizable: Realizability) /: tp.fields)(checkField)
    else
      Realizable
  }
}
