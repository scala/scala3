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

  class HasProblemBounds(typ: SingleDenotation)(implicit ctx: Context)
  extends Realizability(i" has a member $typ with possibly conflicting bounds ${typ.info.bounds.lo} <: ... <: ${typ.info.bounds.hi}")

  class HasProblemBaseArg(typ: Type, argBounds: TypeBounds)(implicit ctx: Context)
  extends Realizability(i" has a base type $typ with possibly conflicting parameter bounds ${argBounds.lo} <: ... <: ${argBounds.hi}")

  class HasProblemBase(base1: Type, base2: Type)(implicit ctx: Context)
  extends Realizability(i" has conflicting base types $base1 and $base2")

  class HasProblemField(fld: SingleDenotation, problem: Realizability)(implicit ctx: Context)
  extends Realizability(i" has a member $fld which is not a legal path\n since ${fld.symbol.name}: ${fld.info}${problem.msg}")

  class ProblemInUnderlying(tp: Type, problem: Realizability)(implicit ctx: Context)
  extends Realizability(i"s underlying type ${tp}${problem.msg}") {
    assert(problem != Realizable)
  }

  def realizability(tp: Type)(implicit ctx: Context) =
    new CheckRealizable().realizability(tp)

  def boundsRealizability(tp: Type)(implicit ctx: Context) =
    new CheckRealizable().boundsRealizability(tp)
}

/** Compute realizability status */
class CheckRealizable(implicit ctx: Context) {
  import CheckRealizable._

  /** A set of all fields that have already been checked. Used
   *  to avoid infinite recursions when analyzing recursive types.
   */
  private val checkedFields: mutable.Set[Symbol] = mutable.LinkedHashSet[Symbol]()

  /** Is symbol's definitition a lazy val?
   *  (note we exclude modules here, because their realizability is ensured separately)
   */
  private def isLateInitialized(sym: Symbol) = sym.is(Lazy, butNot = Module)

  /** The realizability status of given type `tp`*/
  def realizability(tp: Type): Realizability = tp.dealias match {
    case tp: TermRef =>
      val sym = tp.symbol
      if (sym.is(Stable)) realizability(tp.prefix)
      else {
        val r =
          if (!sym.isStable) NotStable
          else if (!isLateInitialized(sym)) realizability(tp.prefix)
          else if (!sym.isEffectivelyFinal) new NotFinal(sym)
          else realizability(tp.info).mapError(r => new ProblemInUnderlying(tp.info, r))
        if (r == Realizable) sym.setFlag(Stable)
        r
      }
    case _: SingletonType | NoPrefix =>
      Realizable
    case tp =>
      def isConcrete(tp: Type): Boolean = tp.dealias match {
        case tp: TypeRef => tp.symbol.isClass
        case tp: TypeProxy => isConcrete(tp.underlying)
        case tp: AndOrType => isConcrete(tp.tp1) && isConcrete(tp.tp2)
        case _ => false
      }
      if (!isConcrete(tp)) NotConcrete
      else boundsRealizability(tp).andAlso(memberRealizability(tp))
  }

  /** `Realizable` if `tp` has good bounds, a `HasProblem...` instance
   *  pointing to a bad bounds member otherwise. "Has good bounds" means:
   *
   *    - all type members have good bounds
   *    - all base types are class types, and if their arguments are wildcards
   *      they have good bounds.
   */
  private def boundsRealizability(tp: Type) = {
    val mbrProblems =
      for {
        mbr <- tp.nonClassTypeMembers
        if !(mbr.info.loBound <:< mbr.info.hiBound)
      }
      yield new HasProblemBounds(mbr)
      
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
      
    (((Realizable: Realizability)
      /: mbrProblems)(_ andAlso _)
      /: baseProblems)(_ andAlso _)
  }

  /** `Realizable` if all of `tp`'s non-struct fields have realizable types,
   *  a `HasProblemField` instance pointing to a bad field otherwise.
   */
  private def memberRealizability(tp: Type) = {
    def checkField(sofar: Realizability, fld: SingleDenotation): Realizability =
      sofar andAlso {
        if (checkedFields.contains(fld.symbol) || fld.symbol.is(Private | Mutable | Lazy))
          // if field is private it cannot be part of a visible path
          // if field is mutable it cannot be part of a path
          // if field is lazy it does not need to be initialized when the owning object is
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
