package dotty.tools.pc.completions

import scala.meta.internal.metals.Fuzzy

import dotty.tools.dotc.ast.tpd.*
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

/** SIP-80 leading-dot completions: when the cursor is in a target-typed
 *  position immediately after `.`, suggest term-level members of the expected
 *  type's companion object. Mirrors `SingletonCompletions` in shape.
 */
object RelativeAdtCompletions:

  def contribute(
      path: List[Tree],
      expectedTypeOpt: Option[Type],
      completionPos: CompletionPos
  )(using ctx: Context): List[CompletionValue] =
    // Determine the target type. Prefer the type we can derive from the path
    // (the qualifier of `.X` after `Typer.typedRelativeSelect`); fall back to
    // the InferCompletionType result, which handles term contexts but not all
    // pattern shapes.
    val expectedTypeFromPath: Option[Type] = path.headOption.flatMap {
      case Select(qual, _) if qual.symbol.is(Flags.Module) =>
        // `qual.symbol` is the companion module produced by Typer.typedRelativeSelect.
        // Recover the corresponding type from its sourceModule.
        val mod = qual.symbol
        val cls = mod.companionClass
        if cls.exists then Some(cls.typeRef) else None
      case _ => None
    }
    val expectedType = expectedTypeOpt.orElse(expectedTypeFromPath)
    expectedType match
      case None => Nil
      case Some(target) =>
        if !isLeadingDotContext(completionPos, path, target) then Nil
        else
          val query = completionPos.query
          val companion = companionOf(target)
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
            val range = completionPos.toEditRange
            members.map { sym =>
              CompletionValue.SingletonValue(sym.name.toString, sym.info, Some(range))
            }

  /** A leading-dot context exists when both:
   *    1. The source has `.` immediately before the query, and
   *    2. The path's head is a Select whose qualifier resolves to the
   *       expected type's companion module (this rules out parse-recovery
   *       cases like `paint(.@@)` where the parser produced `paint(Predef.???)`).
   *
   *  Note: we used to also reject the case where the character preceding the
   *  dot was an identifier-part to filter out method-chain selection. That's
   *  redundant — a method-chain `expr.partial` would never produce a Select
   *  whose qualifier symbol is a Module matching the expected type's
   *  companion, so check (2) already handles it. Dropping the source check
   *  also lets `case .R@@` work (where `case` precedes the dot).
   */
  private def isLeadingDotContext(
      completionPos: CompletionPos,
      path: List[Tree],
      expectedType: Type
  )(using Context): Boolean =
    val content = completionPos.originalCursorPosition.source.content()
    val dotIdx = completionPos.queryStart - 1
    val hasDotBefore =
      dotIdx >= 0 && dotIdx < content.length && content(dotIdx) == '.'
    if !hasDotBefore then false
    else
      val companion = companionOf(expectedType)
      companion.exists && path.headOption.exists {
        case Select(qual, _) =>
          // Distinguish SIP-80 desugaring from a user-written `T.X` selection:
          // `Typer.typedRelativeSelect` synthesises the qualifier with a point
          // span at the `.`, so its span has zero width. A user-written
          // qualifier like `TestEnum` has a non-zero span covering its name.
          qual.symbol == companion && qual.span.start == qual.span.end
        case _ => false
      }

  private def companionOf(tp0: Type)(using Context): Symbols.Symbol =
    // Mirror the resolution in `Typer.typedRelativeSelect`: peel `T | Null`
    // (or `Null | T`) to `T`, then prefer the alias's own companion
    // (via prefix.member) so opaque types pick their alias's companion
    // rather than the underlying class's.
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
