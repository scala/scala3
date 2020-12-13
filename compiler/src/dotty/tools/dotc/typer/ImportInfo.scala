package dotty.tools
package dotc
package typer

import ast.{tpd, untpd}
import ast.Trees._
import core._
import printing.{Printer, Showable}
import util.SimpleIdentityMap
import Symbols._, Names._, Types._, Contexts._, StdNames._, Flags._
import Implicits.RenamedImplicitRef
import config.SourceVersion
import StdNames.nme
import printing.Texts.Text
import ProtoTypes.NoViewsAllowed.normalizedCompatible
import Decorators._

object ImportInfo {

  case class RootRef(refFn: () => TermRef, isPredef: Boolean = false)

  /** The import info for a root import */
  def rootImport(ref: RootRef)(using Context): ImportInfo =
    var selectors =
      untpd.ImportSelector(untpd.Ident(nme.WILDCARD))  // import all normal members...
      :: untpd.ImportSelector(untpd.Ident(nme.EMPTY))  // ... and also all given members
      :: Nil
    if ref.isPredef then                               // do not import any2stringadd
      selectors = untpd.ImportSelector(untpd.Ident(nme.any2stringadd), untpd.Ident(nme.WILDCARD))
        :: selectors

    def sym(using Context) =
      val expr = tpd.Ident(ref.refFn()) // refFn must be called in the context of ImportInfo.sym
      tpd.Import(expr, selectors).symbol

    ImportInfo(sym, selectors, None, isRootImport = true)

  extension (c: Context)
    def withRootImports(rootRefs: List[RootRef])(using Context): Context =
      rootRefs.foldLeft(c)((ctx, ref) => ctx.fresh.setImportInfo(rootImport(ref)))

    def withRootImports: Context =
      given Context = c
      c.withRootImports(defn.rootImportFns)

}

/** Info relating to an import clause
 *  @param   sym           The import symbol defined by the clause
 *  @param   selectors     The selector clauses
 *  @param   symNameOpt    Optionally, the name of the import symbol. None for root imports.
 *                         Defined for all explicit imports from ident or select nodes.
 *  @param   isRootImport  true if this is one of the implicit imports of scala, java.lang,
 *                         scala.Predef in the start context, false otherwise.
 */
class ImportInfo(symf: Context ?=> Symbol,
                 val selectors: List[untpd.ImportSelector],
                 symNameOpt: Option[TermName],
                 val isRootImport: Boolean = false) extends Showable {

  def sym(using Context): Symbol = {
    if (mySym == null) {
      mySym = symf
      assert(mySym != null)
    }
    mySym
  }
  private var mySym: Symbol = _

  /** The (TermRef) type of the qualifier of the import clause */
  def site(using Context): Type = sym.info match {
    case ImportType(expr) => expr.tpe
    case _ => NoType
  }

  /** The names that are excluded from any wildcard import */
  def excluded: Set[TermName] = { ensureInitialized(); myExcluded }

  /** A mapping from original to renamed names */
  def forwardMapping: SimpleIdentityMap[TermName, TermName] = { ensureInitialized(); myForwardMapping }

  /** A mapping from renamed to original names */
  def reverseMapping: SimpleIdentityMap[TermName, TermName] = { ensureInitialized(); myReverseMapping }

  /** Does the import clause contain wildcard selectors (both `_` and `given` count)? */
  def isWildcardImport: Boolean = { ensureInitialized(); myWildcardImport }

  /** Does the import clause have at least one `given` selector? */
  def isGivenImport: Boolean = { ensureInitialized(); myGivenImport }

  private var myExcluded: Set[TermName] = null
  private var myForwardMapping: SimpleIdentityMap[TermName, TermName] = null
  private var myReverseMapping: SimpleIdentityMap[TermName, TermName] = null
  private var myWildcardImport: Boolean = false
  private var myGivenImport: Boolean = false
  private var myWildcardBound: Type = NoType
  private var myGivenBound: Type = NoType

  /** Compute info relating to the selector list */
  private def ensureInitialized(): Unit = if myExcluded == null then
    myExcluded = Set()
    myForwardMapping = SimpleIdentityMap.empty
    myReverseMapping = SimpleIdentityMap.empty
    for sel <- selectors do
      if sel.isWildcard then
        myWildcardImport = true
        if sel.isGiven then myGivenImport = true
      else
        if sel.rename != sel.name then
          myExcluded += sel.name
        if sel.rename != nme.WILDCARD then
          myForwardMapping = myForwardMapping.updated(sel.name, sel.rename)
          myReverseMapping = myReverseMapping.updated(sel.rename, sel.name)

  /** The upper bound for `given` wildcards, or `Nothing` if there are none */
  def givenBound(using Context) =
    if !myGivenBound.exists then
      myGivenBound = ctx.typer.importBound(selectors, isGiven = true)
    myGivenBound

  /** The upper bound for `_` wildcards, or `Nothing` if there are none */
  def wildcardBound(using Context) =
    if !myWildcardBound.exists then
      myWildcardBound = ctx.typer.importBound(selectors, isGiven = false)
    myWildcardBound

  /** The implicit references imported by this import clause */
  def importedImplicits(using Context): List[ImplicitRef] =
    val pre = site
    if isWildcardImport then
      pre.implicitMembers.flatMap { ref =>
        val name = ref.name.toTermName
        if excluded.contains(name) then Nil
        else
          val renamed = forwardMapping(ref.name)
          if renamed == null then // not explicitly named
            val isEligible =
              isGivenImport                             // a given import pulls in implicits and givens
              || ctx.mode.is(Mode.FindHiddenImplicits)  // consider both implicits and givens for error reporting
              || ref.symbol.is(Implicit)                // a wildcard `_` import only pulls in implicits
            val bound = if isGivenImport then givenBound else wildcardBound
            if isEligible && ref.denot.asSingleDenotation.matchesImportBound(bound) then ref :: Nil
            else Nil
          else if renamed == ref.name then ref :: Nil
          else RenamedImplicitRef(ref, renamed) :: Nil
      }
    else
      for
        renamed <- reverseMapping.keys
        denot <- pre.member(reverseMapping(renamed)).altsWith(_.isOneOf(GivenOrImplicit))
      yield
        val original = reverseMapping(renamed)
        val ref = TermRef(pre, original, denot)
        if renamed == original then ref
        else RenamedImplicitRef(ref, renamed)

  /** The root import symbol hidden by this symbol, or NoSymbol if no such symbol is hidden.
   *  Note: this computation needs to work even for un-initialized import infos, and
   *  is not allowed to force initialization.
   *
   *  TODO: Once we have fully bootstrapped, I would prefer if we expressed
   *  unimport with an `override` modifier, and generalized it to all imports.
   *  I believe this would be more transparent than the current set of conditions. E.g.
   *
   *      override import Predef.{any2stringAdd => _, StringAdd => _, _} // disables String +
   *      override import java.lang.{}                                   // disables all imports
   */
  def unimported(using Context): Symbol =
    if myUnimported == null then
      lazy val sym = site.termSymbol
      def maybeShadowsRoot = symNameOpt match
        case Some(symName) => defn.ShadowableImportNames.contains(symName)
        case None => false
      myUnimported =
        if maybeShadowsRoot && defn.rootImportTypes.exists(_.symbol == sym) then sym
        else NoSymbol
      assert(myUnimported != null)
    myUnimported

  private var myUnimported: Symbol = _

  private var myOwner: Symbol = null
  private var myResults: SimpleIdentityMap[TermName, java.lang.Boolean] = SimpleIdentityMap.empty

  /** Does this import clause or a preceding import clause import `owner.feature`? */
  def featureImported(feature: TermName, owner: Symbol)(using Context): Boolean =

    def compute =
      val isImportOwner = site.typeSymbol.eq(owner)
      if isImportOwner && forwardMapping.contains(feature) then true
      else if isImportOwner && excluded.contains(feature) then false
      else
        var c = ctx.outer
        while c.importInfo eq ctx.importInfo do c = c.outer
        (c.importInfo != null) && c.importInfo.featureImported(feature, owner)(using c)

    if myOwner.ne(owner) || !myResults.contains(feature) then
      myOwner = owner
      myResults = myResults.updated(feature, compute)
    myResults(feature)
  end featureImported

  def toText(printer: Printer): Text = printer.toText(this)
}
