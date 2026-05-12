package dotty.tools
package dotc
package typer

import config.Feature
import core.*
import Contexts.*
import Decorators.*
import Flags.*
import Names.*
import NameOps.*
import StdNames.nme
import Symbols.*
import Types.*
import ProtoTypes.*
import ast.untpd

/** Shared helpers for SIP-80 companion scope inference.
 *
 *  Both `Typer.tryCompanionScopeInference` (the resolution fallback) and
 *  `ErrorReporting.typeMismatch` (the silent-shadow diagnostic hint) use the
 *  same expected-type reduction and companion lookup, so they live here.
 */
object CompanionScopeInference:

  /** Reduce `pt` to its principal class type for the purposes of companion
   *  lookup. Returns `NoType` when no useful target can be determined
   *  (proto types without a result, wildcard, uninstantiated type vars,
   *  type bounds, non-`Null` unions).
   */
  def principalTarget(tp: Type)(using Context): Type = tp match
    case tp: SelectionProto => principalTarget(tp.memberProto)
    case tp: IgnoredProto   => principalTarget(tp.ignored)
    case tp: FunProto       => principalTarget(tp.resultType)
    case tp: PolyProto      => principalTarget(tp.resType)
    case tp: ProtoType      => NoType
    case tp: TypeBounds     => NoType
    case tp: TypeVar =>
      val inst = tp.instanceOpt
      if inst.exists then principalTarget(inst)
      else
        val hi = TypeComparer.fullUpperBound(tp.origin)
        if !hi.exists || hi.isExactlyAny then NoType
        else principalTarget(hi)
    case tp if tp eq WildcardType => NoType
    case tp =>
      val w = tp.widenExpr
      w match
        case w: TypeVar => principalTarget(w)
        case OrType(lhs, rhs) if rhs.classSymbol == defn.NullClass => principalTarget(lhs)
        case OrType(lhs, rhs) if lhs.classSymbol == defn.NullClass => principalTarget(rhs)
        case _ => w.dropDependentRefinement

  /** The companion module to search for `target`'s SIP-80 lookup. Prefers
   *  the alias's own companion via `prefix.member(name)` (so opaque type
   *  aliases pick their own companion), falling back to
   *  `classSymbol.companionModule`.
   */
  def companionFor(target: Type)(using Context): Symbol =
    val byPrefix = target match
      case ref: TypeRef =>
        val prefix = ref.prefix
        if prefix.exists && prefix.ne(NoPrefix) then
          val mem = prefix.member(ref.name.toTermName)
          if mem.exists then mem.suchThat(_.is(Module)).symbol else NoSymbol
        else NoSymbol
      case _ => NoSymbol
    if byPrefix.exists then byPrefix
    else
      val cls = target.classSymbol
      if cls.exists then cls.companionModule else NoSymbol

  /** Is `member` an anonymous given (and therefore ineligible per SIP-80)? */
  def isAnonymousGiven(sym: Symbol)(using Context): Boolean =
    sym.is(Given) && sym.name.toString.startsWith("given_")

  /** Source-style fully qualified path for `companion` — `Foo.Bar` rather
   *  than the module-class form `Foo$.Bar` that `Symbol.fullName` returns. */
  private def companionPath(companion: Symbol)(using Context): String =
    companion.showFullName

  /** Render a hint pointing the user at `companion.name` when companion
   *  inference would have produced a different (better) result than normal
   *  resolution. Returns `None` if no such alternative exists.
   */
  def shadowingHint(name: Name, pt: Type)(using Context): Option[String] =
    if !Feature.enabled(Feature.companionScopeInference) then None
    else if !name.isTermName then None
    else
      val target = principalTarget(pt)
      if !target.exists then None
      else
        val companion = companionFor(target)
        if !companion.exists then None
        else
          val termName = name.toTermName
          val member = companion.info.member(termName)
          if !member.exists || isAnonymousGiven(member.symbol) then None
          else
            val path = companionPath(companion)
            Some:
              i"\n\nNote: `$path` has a member named `$termName`; qualify as `$path.$termName` if you meant that."

  /** Render a hint for a "not found" error after companion scope inference also
   *  failed: name the companion that was searched. */
  def notFoundHint(name: Name, pt: Type)(using Context): Option[String] =
    if !Feature.enabled(Feature.companionScopeInference) then None
    else
      val target = principalTarget(pt)
      if !target.exists then None
      else
        val companion = companionFor(target)
        if !companion.exists then None
        else Some:
          i"\n\nSearched expected type `$target`'s companion; no term-level member named `$name`."

end CompanionScopeInference
