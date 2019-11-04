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
import printing.Texts.Text
import ProtoTypes.NoViewsAllowed.normalizedCompatible
import Decorators._

object ImportInfo {

  type RootRef = (
    () => TermRef,  // a lazy reference to the root module to be imported
    Boolean         // true if this will refer to scala.Predef
  )

  /** The import info for a root import from given symbol `sym` */
  def rootImport(rootRef: RootRef)(implicit ctx: Context): ImportInfo =
    val (refFn, isPredef) = rootRef
    var selectors =
      untpd.ImportSelector(untpd.Ident(nme.WILDCARD))  // import all normal members...
      :: untpd.ImportSelector(untpd.Ident(nme.EMPTY))  // ... and also all given members
      :: Nil
    if isPredef then                                   // do not import any2stringadd
      selectors = untpd.ImportSelector(untpd.Ident(nme.any2stringadd), untpd.Ident(nme.WILDCARD))
        :: selectors
    def expr(implicit ctx: Context) = tpd.Ident(refFn())
    def imp(implicit ctx: Context) = tpd.Import(expr, selectors)
    ImportInfo(imp.symbol, selectors, None, isRootImport = true)
}

/** Info relating to an import clause
 *  @param   sym           The import symbol defined by the clause
 *  @param   selectors     The selector clauses
 *  @param   symNameOpt    Optionally, the name of the import symbol. None for root imports.
 *                         Defined for all explicit imports from ident or select nodes.
 *  @param   isRootImport  true if this is one of the implicit imports of scala, java.lang,
 *                         scala.Predef or dotty.DottyPredef in the start context, false otherwise.
 */
class ImportInfo(symf: (given Context) => Symbol,
                 val selectors: List[untpd.ImportSelector],
                 symNameOpt: Option[TermName],
                 val isRootImport: Boolean = false) extends Showable {

  // Dotty deviation: we cannot use a lazy val here for the same reason
  // that we cannot use one for `DottyPredefModuleRef`.
  def sym(implicit ctx: Context): Symbol = {
    if (mySym == null) {
      mySym = symf(given ctx)
      assert(mySym != null)
    }
    mySym
  }
  private var mySym: Symbol = _

  /** The (TermRef) type of the qualifier of the import clause */
  def site(implicit ctx: Context): Type = sym.info match {
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
    myForwardMapping = SimpleIdentityMap.Empty
    myReverseMapping = SimpleIdentityMap.Empty
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
  def givenBound(implicit ctx: Context) =
    if !myGivenBound.exists then
      myGivenBound = ctx.typer.importBound(selectors, isGiven = true)
    myGivenBound

  /** The upper bound for `_` wildcards, or `Nothing` if there are none */
  def wildcardBound(implicit ctx: Context) =
    if !myWildcardBound.exists then
      myWildcardBound = ctx.typer.importBound(selectors, isGiven = false)
    myWildcardBound

  /** The implicit references imported by this import clause */
  def importedImplicits(implicit ctx: Context): List[ImplicitRef] =
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
  def unimported(implicit ctx: Context): Symbol =
    if myUnimported == null then
      lazy val sym = site.termSymbol
      def maybeShadowsRoot = symNameOpt match
        case Some(symName) => defn.ShadowableImportNames.contains(symName)
        case None => false
      myUnimported =
        if maybeShadowsRoot && defn.RootImportTypes.exists(_.symbol == sym) then sym
        else NoSymbol
      assert(myUnimported != null)
    myUnimported

  private var myUnimported: Symbol = _

  /** Does this import clause or a preceding import clause import `owner.feature`? */
  def featureImported(feature: TermName, owner: Symbol)(implicit ctx: Context): Boolean =

    def compute =
      val isImportOwner = site.widen.typeSymbol.eq(owner)
      if isImportOwner && forwardMapping.contains(feature) then true
      else if isImportOwner && excluded.contains(feature) then false
      else
        var c = ctx.outer
        while c.importInfo eq ctx.importInfo do c = c.outer
        (c.importInfo != null) && c.importInfo.featureImported(feature, owner)(c)

    if (lastOwner.ne(owner) || !lastResults.contains(feature)) {
      lastOwner = owner
      lastResults = lastResults.updated(feature, compute)
    }
    lastResults(feature)

  private var lastOwner: Symbol = null
  private var lastResults: SimpleIdentityMap[TermName, java.lang.Boolean] = SimpleIdentityMap.Empty

  def toText(printer: Printer): Text = printer.toText(this)
}