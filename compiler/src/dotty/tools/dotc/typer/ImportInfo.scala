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

object ImportInfo {
  /** The import info for a root import from given symbol `sym` */
  def rootImport(refFn: () => TermRef)(implicit ctx: Context): ImportInfo = {
    val selectors = untpd.Ident(nme.WILDCARD) :: Nil
    def expr(implicit ctx: Context) = tpd.Ident(refFn())
    def imp(implicit ctx: Context) = tpd.Import(importImplied = false, expr, selectors)
    new ImportInfo(implicit ctx => imp.symbol, selectors, None, importImplied = false, isRootImport = true)
  }
}

/** Info relating to an import clause
 *  @param   sym           The import symbol defined by the clause
 *  @param   selectors     The selector clauses
 *  @param   symNameOpt    Optionally, the name of the import symbol. None for root imports.
 *                         Defined for all explicit imports from ident or select nodes.
 *  @param   importImplied true if this is an implied import
 *  @param   isRootImport  true if this is one of the implicit imports of scala, java.lang,
 *                         scala.Predef or dotty.DottyPredef in the start context, false otherwise.
 */
class ImportInfo(symf: Context => Symbol, val selectors: List[untpd.Tree],
                 symNameOpt: Option[TermName],
                 val importImplied: Boolean,
                 val isRootImport: Boolean = false) extends Showable {

  // Dotty deviation: we cannot use a lazy val here for the same reason
  // that we cannot use one for `DottyPredefModuleRef`.
  def sym(implicit ctx: Context): Symbol = {
    if (mySym == null) {
      mySym = symf(ctx)
      assert(mySym != null)
    }
    mySym
  }
  private[this] var mySym: Symbol = _

  /** The (TermRef) type of the qualifier of the import clause */
  def site(implicit ctx: Context): Type = {
    val ImportType(expr) = sym.info
    expr.tpe
  }

  /** The names that are excluded from any wildcard import */
  def excluded: Set[TermName] = { ensureInitialized(); myExcluded }

  /** A mapping from renamed to original names */
  def reverseMapping: SimpleIdentityMap[TermName, TermName] = { ensureInitialized(); myMapped }

  /** The original names imported by-name before renaming */
  def originals: Set[TermName] = { ensureInitialized(); myOriginals }

  /** Does the import clause end with wildcard? */
  def isWildcardImport: Boolean = { ensureInitialized(); myWildcardImport }

  private[this] var myExcluded: Set[TermName] = null
  private[this] var myMapped: SimpleIdentityMap[TermName, TermName] = null
  private[this] var myOriginals: Set[TermName] = null
  private[this] var myWildcardImport: Boolean = false

  /** Compute info relating to the selector list */
  private def ensureInitialized(): Unit = if (myExcluded == null) {
    myExcluded = Set()
    myMapped = SimpleIdentityMap.Empty
    myOriginals = Set()
    def recur(sels: List[untpd.Tree]): Unit = sels match {
      case sel :: sels1 =>
        sel match {
          case Thicket(Ident(name: TermName) :: Ident(nme.WILDCARD) :: Nil) =>
            myExcluded += name
          case Thicket(Ident(from: TermName) :: Ident(to: TermName) :: Nil) =>
            myMapped = myMapped.updated(to, from)
            myExcluded += from
            myOriginals += from
          case Ident(nme.WILDCARD) =>
            myWildcardImport = true
          case Ident(name: TermName) =>
            myMapped = myMapped.updated(name, name)
            myOriginals += name
        }
        recur(sels1)
      case nil =>
    }
    recur(selectors)
  }

  private def implicitFlag = if (importImplied) Implied else Implicit

  /** The implicit references imported by this import clause */
  def importedImplicits(implicit ctx: Context): List[ImplicitRef] = {
    val pre = site
    if (isWildcardImport) {
      val refs = pre.implicitMembers(implicitFlag)
      if (excluded.isEmpty) refs
      else refs filterNot (ref => excluded contains ref.name.toTermName)
    } else
      for {
        renamed <- reverseMapping.keys
        denot <- pre.member(reverseMapping(renamed)).altsWith(_ is implicitFlag)
      } yield {
        val original = reverseMapping(renamed)
        val ref = TermRef(pre, original, denot)
        if (renamed == original) ref
        else new RenamedImplicitRef(ref, renamed)
      }
  }

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
  def unimported(implicit ctx: Context): Symbol = {
    if (myUnimported == null) {
      lazy val sym = site.termSymbol
      def maybeShadowsRoot = symNameOpt match {
        case Some(symName) => defn.ShadowableImportNames.contains(symName)
        case None => false
      }
      myUnimported =
        if (maybeShadowsRoot && defn.RootImportTypes.exists(_.symbol == sym)) sym
        else NoSymbol
      assert(myUnimported != null)
    }
    myUnimported
  }
  private[this] var myUnimported: Symbol = _

  /** Does this import clause or a preceding import clause import `owner.feature`? */
  def featureImported(owner: Symbol, feature: TermName)(implicit ctx: Context): Boolean = {
    def compute = {
      val isImportOwner = site.widen.typeSymbol `eq` owner
      if (isImportOwner && originals.contains(feature)) true
      else if (isImportOwner && excluded.contains(feature)) false
      else {
        var c = ctx.outer
        while (c.importInfo eq ctx.importInfo) c = c.outer
        (c.importInfo != null) && c.importInfo.featureImported(owner, feature)(c)
      }
    }
    if (lastOwner.ne(owner) || !lastResults.contains(feature)) {
      lastOwner = owner
      lastResults = lastResults.updated(feature, compute)
    }
    lastResults(feature)
  }

  private[this] var lastOwner: Symbol = null
  private[this] var lastResults: SimpleIdentityMap[TermName, java.lang.Boolean] = SimpleIdentityMap.Empty

  def toText(printer: Printer): Text = printer.toText(this)
}
