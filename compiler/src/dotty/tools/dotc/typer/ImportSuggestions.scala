package dotty.tools
package dotc
package typer

import core._
import Contexts._, Types._, Symbols._, Names._, Decorators._, ProtoTypes._
import Flags._
import NameKinds.FlatName
import config.Printers.implicits
import util.Spans.Span
import ast.{untpd, tpd}
import Implicits.{hasExtMethod, Candidate}
import java.util.{Timer, TimerTask}
import collection.mutable

/** This trait defines the method `importSuggestionAddendum` that adds an addendum
 *  to error messages suggesting additional imports.
 */
trait ImportSuggestions with
  this: Typer =>

  import tpd._

  /** Timeout to test a single implicit value as a suggestion, in ms */
  private inline val testOneImplicitTimeOut = 500

  /** Global timeout to stop looking for further implicit suggestions, in ms */
  private inline val suggestImplicitTimeOut = 10000

  /** A list of TermRefs referring to the roots where suggestions for
   *  imports of givens or extension methods that might fix a type error
   *  are searched.
   *
   *  These roots are the smallest set of objects and packages that includes
   *
   *   - any object that is a defined in an enclosing scope,
   *   - any object that is a member of an enclosing class,
   *   - any enclosing package (including the root package),
   *   - any object that is a member of a searched object or package,
   *   - any object or package from which something is imported in an enclosing scope,
   *   - any package that is nested in a searched package, provided
   *     the package was accessed in some way previously.
   *
   *  Excluded from the root set are:
   *
   *   - Objects that contain `$`s in their name. These have to
   *     be omitted since they might be inner Java class files which
   *     cannot be read by the ClassfileParser without crashing.
   *   - Any members of static parts of Java classes.
   *   - Any members of the empty package. These should be
   *     skipped since the empty package often contains unrelated junk files
   *     that should not be used for suggestions.
   *   - Any members of the java or java.lang packages. These are
   *     skipped as an optimization, since they won't contain implicits anyway.
   */
  private def suggestionRoots(given Context) =
    val seen = mutable.Set[TermRef]()

    def lookInside(root: Symbol)(given Context): Boolean =
      if root.is(Package) then root.isTerm && root.isCompleted
      else !root.name.is(FlatName)
        && !root.name.lastPart.contains('$')
        && root.is(ModuleVal, butNot = JavaDefined)

    def nestedRoots(site: Type)(given Context): List[Symbol] =
      val seenNames = mutable.Set[Name]()
      site.baseClasses.flatMap { bc =>
        bc.info.decls.filter { dcl =>
          lookInside(dcl)
          && !seenNames.contains(dcl.name)
          && { seenNames += dcl.name; true }
        }
      }

    def rootsStrictlyIn(ref: Type)(given Context): List[TermRef] =
      val site = ref.widen
      val refSym = site.typeSymbol
      val nested =
        if refSym.is(Package) then
          if refSym == defn.EmptyPackageClass       // Don't search the empty package
              || refSym == defn.JavaPackageClass     // As an optimization, don't search java...
              || refSym == defn.JavaLangPackageClass // ... or java.lang.
          then Nil
          else refSym.info.decls.filter(lookInside)
        else
          if !refSym.is(Touched) then refSym.ensureCompleted() // JavaDefined is reliably known only after completion
          if refSym.is(JavaDefined) then Nil
          else nestedRoots(site)
      nested
        .map(mbr => TermRef(ref, mbr.asTerm))
        .flatMap(rootsIn)
        .toList

    def rootsIn(ref: TermRef)(given Context): List[TermRef] =
      if seen.contains(ref) then Nil
      else
        implicits.println(i"search for suggestions in ${ref.symbol.fullName}")
        seen += ref
        ref :: rootsStrictlyIn(ref)

    def rootsOnPath(tp: Type)(given Context): List[TermRef] = tp match
      case ref: TermRef => rootsIn(ref) ::: rootsOnPath(ref.prefix)
      case _ => Nil

    def recur(given ctx: Context): List[TermRef] =
      if ctx.owner.exists then
        val defined =
          if ctx.owner.isClass then
            if ctx.owner eq ctx.outer.owner then Nil
            else rootsStrictlyIn(ctx.owner.thisType)
          else
            if ctx.scope eq ctx.outer.scope then Nil
            else ctx.scope
              .filter(lookInside(_))
              .flatMap(sym => rootsIn(sym.termRef))
        val imported =
          if ctx.importInfo eq ctx.outer.importInfo then Nil
          else ctx.importInfo.sym.info match
            case ImportType(expr) => rootsOnPath(expr.tpe)
            case _ => Nil
        defined ++ imported ++ recur(given ctx.outer)
      else Nil

    recur
  end suggestionRoots

  /** Given an expected type `pt`, return two lists of TermRefs:
   *
   *   1. The _fully matching_ given instances that can be completed
   *      to a full synthesized given term that matches the expected type `pt`.
   *
   *   2. The _head matching_ given instances, that conform to the
   *      expected type `pt`, ignoring any dependent implicit arguments.
   *
   *   If there are no fully matching given instances under (1), and `pt` is
   *   a view prototype of a selection of the form `T ?=>? { name: ... }`,
   *   return instead a list of all possible references to extension methods named
   *   `name` that are applicable to `T`.
   */
  private def importSuggestions(pt: Type)(given ctx: Context): (List[TermRef], List[TermRef]) =
    val timer = new Timer()
    val deadLine = System.currentTimeMillis() + suggestImplicitTimeOut

    /** Test whether the head of a given instance matches the expected type `pt`,
     *  ignoring any dependent implicit arguments.
     */
    def shallowTest(ref: TermRef): Boolean =
      System.currentTimeMillis < deadLine
      && {
        given Context = ctx.fresh.setExploreTyperState()
        pt match
          case pt: ViewProto => pt.isMatchedBy(ref)
          case _ => normalize(ref, pt) <:< pt
      }

    /** Test whether a full given term can be synthesized that matches
     *  the expected type `pt`.
     */
    def deepTest(ref: TermRef): Boolean =
      System.currentTimeMillis < deadLine
      && {
        val task = new TimerTask with
          def run() =
            println(i"Cancelling test of $ref when making suggestions for error in ${ctx.source}")
            ctx.run.isCancelled = true
        val span = ctx.owner.sourcePos.span
        val (expectedType, argument, kind) = pt match
          case ViewProto(argType, resType) =>
            (resType,
             untpd.Ident(ref.name).withSpan(span).withType(argType),
             if hasExtMethod(ref, resType) then Candidate.Extension
             else Candidate.Conversion)
          case _ =>
            (pt, EmptyTree, Candidate.Value)
        val candidate = Candidate(ref, kind, 0)
        try
          timer.schedule(task, testOneImplicitTimeOut)
          typedImplicit(candidate, expectedType, argument, span)(
            given ctx.fresh.setExploreTyperState()).isSuccess
        finally
          task.cancel()
          ctx.run.isCancelled = false
      }
    end deepTest

    /** Optionally, an extension method reference `site.name` that is
     *  applicable to `argType`.
     */
    def extensionMethod(site: TermRef, name: TermName, argType: Type): Option[TermRef] =
      site.member(name)
        .alternatives
        .map(mbr => TermRef(site, mbr.symbol))
        .filter(ref =>
          ref.symbol.is(Extension)
          && isApplicableMethodRef(ref, argType :: Nil, WildcardType))
        .headOption

    try
      val roots = suggestionRoots
        .filterNot(root => defn.RootImportTypes.exists(_.symbol == root.symbol))
          // don't suggest things that are imported by default

      def extensionImports = pt match
        case ViewProto(argType, SelectionProto(name: TermName, _, _, _)) =>
          roots.flatMap(extensionMethod(_, name, argType))
        case _ =>
          Nil

      roots
        .flatMap(_.implicitMembers.filter(shallowTest))
          // filter whether the head of the implicit can match
        .partition(deepTest)
          // partition into full matches and head matches
        match
          case (Nil, partials) => (extensionImports, partials)
          case givenImports => givenImports
    catch
      case ex: Throwable =>
        if ctx.settings.Ydebug.value then
          println("caught exception when searching for suggestions")
          ex.printStackTrace()
        (Nil, Nil)
    finally timer.cancel()
  end importSuggestions

  /** An addendum to an error message where the error might be fixed
   *  by some implicit value of type `pt` that is however not found.
   *  The addendum suggests given imports that might fix the problem.
   *  If there's nothing to suggest, an empty string is returned.
   */
  override def importSuggestionAddendum(pt: Type)(given ctx: Context): String =
    val (fullMatches, headMatches) =
      importSuggestions(pt)(given ctx.fresh.setExploreTyperState())
    implicits.println(i"suggestions for $pt in ${ctx.owner} = ($fullMatches%, %, $headMatches%, %)")
    val (suggestedRefs, help) =
      if fullMatches.nonEmpty then (fullMatches, "fix")
      else (headMatches, "make progress towards fixing")
    def importString(ref: TermRef): String =
      s"  import ${ctx.printer.toTextRef(ref).show}"
    val suggestions = suggestedRefs
      .zip(suggestedRefs.map(importString))
      .filter((ref, str) => str.contains('.'))
      .sortWith { (x, y) =>
          // sort by specificity first, alphabetically second
          val ((ref1, str1), (ref2, str2)) = (x, y)
          val diff = compare(ref1, ref2)
          diff > 0 || diff == 0 && str1 < str2
      }
      .map((ref, str) => str)
      .distinct  // TermRefs might be different but generate the same strings
    if suggestions.isEmpty then ""
    else
      val fix =
        if suggestions.tail.isEmpty then "The following import"
        else "One of the following imports"
      i"""
         |
         |$fix might $help the problem:
         |
         |$suggestions%\n%
         """
  end importSuggestionAddendum
end ImportSuggestions
