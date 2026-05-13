package dotty.tools.pc.completions

import scala.meta.internal.metals.Fuzzy

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.config.Feature
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.NameOps.*
import dotty.tools.dotc.core.Symbols
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.core.Types.NoPrefix
import dotty.tools.dotc.core.Types.OrType
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.core.Types.TypeRef
import dotty.tools.pc.utils.InteractiveEnrichments.*

/** SIP-80 companion-scope-inference completions: when the cursor is at an
 *  identifier (or empty cursor) in a target-typed position, suggest term-level
 *  members of the expected type's companion object alongside the regular
 *  lexical-scope completions.
 *
 *  Mirrors `SingletonCompletions` in shape and is wired alongside it. Unlike
 *  the leading-`.` / leading-`#` variants, there's no sigil to detect — the
 *  contributor fires whenever the path head is an `Ident` with an inferable
 *  expected type whose companion has term members.
 */
object CompanionScopeCompletions:

  def contribute(
      path: List[Tree],
      expectedType: Type,
      completionPos: CompletionPos
  )(using ctx: Context): List[CompletionValue] =
    if !Feature.enabled(Feature.companionScopeInference) then Nil
    else
      path match
        case (ident @ Ident(name)) :: _ =>
          val query = name.toString.replace(Cursor.value, "").nn
          val companion = companionOf(expectedType)
          if !companion.exists then Nil
          else
            val info = companion.info
            val isMatch: Symbols.Symbol => Boolean = sym =>
              sym.isTerm
                && !sym.isConstructor
                && !sym.is(Flags.Synthetic)
                && !sym.is(Flags.Private)
                && !sym.name.toString.contains('$')
                && !isAnonymousGiven(sym)
                && (query.isEmpty || Fuzzy.matches(query, sym.name.toString))
            val members =
              info.decls.iterator
                .filter(isMatch)
                .toList
                .distinctBy(_.name.toString)
            val range = completionPos
              .originalCursorPosition
              .withStart(ident.span.start)
              .withEnd(ident.span.start + query.length)
              .toLsp
            members.map { sym =>
              CompletionValue.SingletonValue(sym.name.toString, sym.info, Some(range))
            }
        case _ => Nil

  /** Mirror `CompanionScopeInference.companionFor`: prefer the alias's own
   *  companion via `prefix.member(name)`, peel `T | Null` first.
   */
  private def companionOf(tp0: Type)(using Context): Symbols.Symbol =
    val tp = peelNullUnion(tp0)
    val byPrefix = tp match
      case ref: TypeRef =>
        val pre = ref.prefix
        if pre.exists && pre.ne(NoPrefix) then
          val mem = pre.member(ref.name.toTermName)
          if mem.exists then mem.suchThat(_.is(Flags.Module)).symbol else NoSymbol
        else NoSymbol
      case _ => NoSymbol
    if byPrefix.exists then byPrefix
    else
      val cls = tp.classSymbol
      if cls.exists then cls.companionModule else NoSymbol

  private def peelNullUnion(tp: Type)(using Context): Type = tp match
    case OrType(lhs, rhs) if rhs.classSymbol == Symbols.defn.NullClass => peelNullUnion(lhs)
    case OrType(lhs, rhs) if lhs.classSymbol == Symbols.defn.NullClass => peelNullUnion(rhs)
    case _ => tp

  private def isAnonymousGiven(sym: Symbols.Symbol)(using Context): Boolean =
    sym.is(Flags.Given) && sym.name.toString.startsWith("given_")
