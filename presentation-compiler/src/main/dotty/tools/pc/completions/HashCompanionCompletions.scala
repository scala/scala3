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

/** SIP-80 `#` companion shorthand completions: when the cursor is in a
 *  target-typed position immediately after `#`, suggest term-level members of
 *  the expected type's companion object. Mirrors `SingletonCompletions` in
 *  shape.
 */
object HashCompanionCompletions:

  def contribute(
      path: List[Tree],
      expectedTypeOpt: Option[Type],
      completionPos: CompletionPos
  )(using ctx: Context): List[CompletionValue] =
    // Determine the target type. Prefer the type derived from the path
    // (the qualifier of `#X` after `Typer.typedHashSelect`); fall back to
    // the InferCompletionType result, which handles term contexts but not
    // all pattern shapes.
    val expectedTypeFromPath: Option[Type] = path.headOption.flatMap {
      case Select(qual, _) if qual.symbol.is(Flags.Module) =>
        val mod = qual.symbol
        val cls = mod.companionClass
        if cls.exists then Some(cls.typeRef) else None
      case _ => None
    }
    val expectedType = expectedTypeOpt.orElse(expectedTypeFromPath)
    expectedType match
      case None => Nil
      case Some(target) =>
        if !isHashContext(completionPos, path, target) then Nil
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

  /** A `#`-shorthand context exists when both:
   *    1. The character immediately before the query is `#`, and
   *    2. The path's head is a Select whose qualifier symbol is the
   *       resolved companion of the expected type and whose qualifier span
   *       has zero width — `Typer.typedHashSelect` synthesises the
   *       qualifier with a point span at the `#`, while a user-written
   *       qualifier like `TestEnum` has a non-zero span covering its name.
   *       This stops the contributor firing on regular selections.
   */
  private def isHashContext(
      completionPos: CompletionPos,
      path: List[Tree],
      expectedType: Type
  )(using Context): Boolean =
    val content = completionPos.originalCursorPosition.source.content()
    val hashIdx = completionPos.queryStart - 1
    val hasHashBefore =
      hashIdx >= 0 && hashIdx < content.length && content(hashIdx) == '#'
    if !hasHashBefore then false
    else
      val companion = companionOf(expectedType)
      companion.exists && path.headOption.exists {
        case Select(qual, _) =>
          qual.symbol == companion && qual.span.start == qual.span.end
        case _ => false
      }

  private def companionOf(tp0: Type)(using Context): Symbols.Symbol =
    // Mirror `Typer.typedHashSelect`: peel `T | Null` (or `Null | T`) to
    // `T`, then prefer the alias's own companion via prefix.member so
    // opaque type aliases pick their alias's companion rather than the
    // underlying class's.
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
