package dotty.tools
package dotc
package reporting

import core.*
import Contexts.*
import Decorators.*, Symbols.*, Names.*, Types.*, Flags.*
import typer.ProtoTypes.SelectionProto

/** A utility object to support "did you mean" hinting */
object DidYouMean:

  def kindOK(sym: Symbol, isType: Boolean, isApplied: Boolean)(using Context): Boolean =
    if isType then sym.isType
    else sym.isTerm || isApplied && sym.isClass && !sym.is(ModuleClass)
      // also count classes if followed by `(` since they have constructor proxies,
      // but these don't show up separately as members
      // Note: One need to be careful here not to complete symbols. For instance,
      // we run into trouble if we ask whether a symbol is a legal value.

  /** The names of all non-synthetic, non-private members of `site`
   *  that are of the same type/term kind as the missing member.
   */
  def memberCandidates(site: Type, isType: Boolean, isApplied: Boolean)(using Context): collection.Set[Symbol] =
    for
      bc <- site.widen.baseClasses.toSet
      sym <- bc.info.decls.filter(sym =>
        kindOK(sym, isType, isApplied)
        && !sym.isConstructor
        && !sym.flagsUNSAFE.isOneOf(Synthetic | Private))
    yield sym

  case class Binding(name: Name, sym: Symbol, site: Type)

  /** The name, symbol, and prefix type of all non-synthetic declarations that are
   *  defined or imported in some enclosing scope and that are of the same type/term
   *  kind as the missing member.
   */
  def inScopeCandidates(isType: Boolean, isApplied: Boolean, rootImportOK: Boolean)(using Context): collection.Set[Binding] =
    val acc = collection.mutable.HashSet[Binding]()
    def nextInteresting(ctx: Context): Context =
      if ctx.outer.isImportContext
        || ctx.outer.scope != ctx.scope
        || ctx.outer.owner.isClass && ctx.outer.owner != ctx.owner
        || (ctx.outer eq NoContext)
      then ctx.outer
      else nextInteresting(ctx.outer)

    def recur()(using Context): Unit =
      if ctx eq NoContext then
        () // done
      else if ctx.isImportContext then
        val imp = ctx.importInfo.nn
        if imp.isRootImport && !rootImportOK then
          () // done
        else imp.importSym.info match
          case ImportType(expr) =>
            val candidates = memberCandidates(expr.tpe, isType, isApplied)
            if imp.isWildcardImport then
              for cand <- candidates if !imp.excluded.contains(cand.name.toTermName) do
                acc += Binding(cand.name, cand, expr.tpe)
            for sel <- imp.selectors do
              val selStr = sel.name.show
              if sel.name == sel.rename then
                for cand <- candidates if cand.name.toTermName.show == selStr do
                  acc += Binding(cand.name, cand, expr.tpe)
              else if !sel.isUnimport then
                for cand <- candidates if cand.name.toTermName.show == selStr do
                  acc += Binding(sel.rename.likeSpaced(cand.name), cand, expr.tpe)
          case _ =>
        recur()(using nextInteresting(ctx))
      else
        if ctx.owner.isClass then
          for sym <- memberCandidates(ctx.owner.typeRef, isType, isApplied) do
            acc += Binding(sym.name, sym, ctx.owner.thisType)
        else
          ctx.scope.foreach: sym =>
            if kindOK(sym, isType, isApplied)
                && !sym.isConstructor
                && !sym.flagsUNSAFE.is(Synthetic)
            then acc += Binding(sym.name, sym, NoPrefix)
        recur()(using nextInteresting(ctx))
    end recur

    recur()
    acc
  end inScopeCandidates

  /** The Levenshtein distance between two strings */
  def distance(s1: String, s2: String): Int =
    val dist = Array.ofDim[Int](s2.length + 1, s1.length + 1)
    for
      j <- 0 to s2.length
      i <- 0 to s1.length
    do
      dist(j)(i) =
        if j == 0 then i
        else if i == 0 then j
        else if s2(j - 1) == s1(i - 1) then dist(j - 1)(i - 1)
        else (dist(j - 1)(i) min dist(j)(i - 1) min dist(j - 1)(i - 1)) + 1
    dist(s2.length)(s1.length)

  /** List of possible candidate names with their Levenshtein distances
   *  to the name `from` of the missing member.
   *  @param maxDist  Maximal number of differences to be considered for a hint
   *                  A distance qualifies if it is at most `maxDist`, shorter than
   *                  the lengths of both the candidate name and the missing member name
   *                  and not greater than half the average of those lengths.
   */
  extension [S <: Symbol | Binding](candidates: collection.Set[S])
    def closestTo(str: String, maxDist: Int = 3)(using Context): List[(Int, S)] =
      def nameStr(cand: S): String = cand match
        case sym: Symbol => sym.name.show
        case bdg: Binding => bdg.name.show
      candidates
        .toList
        .map(cand => (distance(nameStr(cand), str), cand))
        .filter((d, cand) =>
          d <= maxDist
          && d * 4 <= str.length + nameStr(cand).length
          && d < str.length
          && d < nameStr(cand).length)
        .sortBy((d, cand) => (d, nameStr(cand)))  // sort by distance first, alphabetically second

  def didYouMean(candidates: List[(Int, Binding)], proto: Type, prefix: String)(using Context): String =

    def qualifies(b: Binding)(using Context): Boolean =
      try
        val valueOK = proto match
          case _: SelectionProto => true
          case _ => !b.sym.isNoValue
        val accessOK = b.sym.isAccessibleFrom(b.site)
        valueOK && accessOK
      catch case ex: Exception => false
        // exceptions might arise when completing (e.g. malformed class file, or cyclic reference)

    def showName(name: Name, sym: Symbol)(using Context): String =
      if sym.is(ModuleClass) then s"${name.show}.type"
      else name.show

    def alternatives(distance: Int, candidates: List[(Int, Binding)]): List[Binding] = candidates match
      case (d, b) :: rest if d == distance =>
        if qualifies(b) then b :: alternatives(distance, rest) else alternatives(distance, rest)
      case _ =>
        Nil

    def recur(candidates: List[(Int, Binding)]): String = candidates match
      case (d, b) :: rest
      if d != 0 || b.sym.is(ModuleClass) => // Avoid repeating the same name in "did you mean"
        if qualifies(b) then
          def hint(b: Binding) = prefix ++ showName(b.name, b.sym)
          val alts = alternatives(d, rest).filter(_.name != b.name).map(hint).take(3).distinct
          val suffix = if alts.isEmpty then "" else alts.mkString(" or perhaps ", " or ", "?")
          s" - did you mean ${hint(b)}?$suffix"
        else
          recur(rest)
      case _ => ""

    recur(candidates)
  end didYouMean
end DidYouMean