package dotty.tools
package dotc
package typer

import ast.{tpd, untpd}
import ast.Trees._
import core._
import printing.{Printer, Showable}
import util.SimpleMap
import Symbols._, Names._, Denotations._, Types._, Contexts._, StdNames._, Flags._
import Decorators.StringInterpolators

object ImportInfo {
  /** The import info for a root import from given symbol `sym` */
  def rootImport(refFn: () => TermRef)(implicit ctx: Context) = {
    val selectors = untpd.Ident(nme.WILDCARD) :: Nil
    def expr(implicit ctx: Context) = tpd.Ident(refFn())
    def imp(implicit ctx: Context) = tpd.Import(expr, selectors)
    new ImportInfo(implicit ctx => imp.symbol, selectors, None, isRootImport = true)
  }
}

/** Info relating to an import clause
 *  @param   sym          The import symbol defined by the clause
 *  @param   selectors    The selector clauses
 *  @param   symNameOpt   Optionally, the name of the import symbol. None for root imports.
 *                        Defined for all explicit imports from ident or select nodes.
 *  @param   isRootImport true if this is one of the implicit imports of scala, java.lang,
 *                        scala.Predef or dotty.DottyPredef in the start context, false otherwise.
 */
class ImportInfo(symf: Context => Symbol, val selectors: List[untpd.Tree],
                 symNameOpt: Option[TermName], val isRootImport: Boolean = false) extends Showable {

  // Dotty deviation: we cannot use a lazy val here for the same reason
  // that we cannot use one for `DottyPredefModuleRef`.
  def sym(implicit ctx: Context) = {
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
  def reverseMapping: SimpleMap[TermName, TermName] = { ensureInitialized(); myMapped }

  /** The original names imported by-name before renaming */
  def originals: Set[TermName] = { ensureInitialized(); myOriginals }

  /** Does the import clause end with wildcard? */
  def isWildcardImport = { ensureInitialized(); myWildcardImport }

  private var myExcluded: Set[TermName] = null
  private var myMapped: SimpleMap[TermName, TermName] = null
  private var myOriginals: Set[TermName] = null
  private var myWildcardImport: Boolean = false

  /** Compute info relating to the selector list */
  private def ensureInitialized(): Unit = if (myExcluded == null) {
    myExcluded = Set()
    myMapped = SimpleMap.Empty
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

  /** The implicit references imported by this import clause */
  def importedImplicits(implicit ctx: Context): List[TermRef] = {
    val pre = site
    if (isWildcardImport) {
      val refs = pre.implicitMembers
      if (excluded.isEmpty) refs
      else refs filterNot (ref => excluded contains ref.name.toTermName)
    } else
      for {
        renamed <- reverseMapping.keys
        denot <- pre.member(reverseMapping(renamed)).altsWith(_ is Implicit)
      } yield TermRef(pre, renamed, denot)
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

  def toText(printer: Printer) = printer.toText(this)
}
