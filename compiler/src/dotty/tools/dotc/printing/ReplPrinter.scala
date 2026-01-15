package dotty.tools.dotc.printing

import dotty.tools.dotc.core.Constants
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.NameOps.*
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.printing.Texts.*
import dotty.tools.dotc.typer.ImportInfo

class ReplPrinter(_ctx: Context) extends RefinedPrinter(_ctx) {

  val debugPrint = _ctx.settings.YprintDebug.value

  private def importSiteMatches(site: Type, owner: Symbol)(using Context): Boolean =
    site.exists && (site.termSymbol == owner || site.termSymbol.moduleClass == owner)

  private def isRootImport(owner: Symbol)(using Context): Boolean =
    defn.rootImportTypes.exists(r => r.symbol == owner || r.symbol.moduleClass == owner)

  private def importedNameFrom(info: ImportInfo, name: Name)(using Context): Name | Null =
    val termName = name.toTermName
    if info.forwardMapping.contains(termName) then info.forwardMapping(termName)
    else if info.isWildcardImport && !info.excluded.contains(termName) then termName
    else null

  private def findImportedName(sym: Symbol, owner: Symbol)(using Context): Name | Null =
    if isRootImport(owner) then sym.name.toTermName
    else
      var c = ctx
      while c ne NoContext do
        if c.isImportContext && importSiteMatches(c.importInfo.nn.site, owner) then
          val result = importedNameFrom(c.importInfo.nn, sym.name)
          if result != null then return result

        c = c.outer
      null

  private def shortRefNameViaImports(tp: TermRef)(using Context): Name | Null =
    if debugPrint then null
    else if !tp.symbol.exists then null
    else if !tp.prefix.termSymbol.exists then null
    else findImportedName(tp.symbol, tp.prefix.termSymbol)

  private def getImportedTypeName(sym: Symbol)(using Context): Name | Null =
    if !sym.exists then null
    else findImportedName(sym, sym.effectiveOwner)

  override def toTextRef(tp: SingletonType): Text = controlled {
    tp match
      case tp: TermRef if !debugPrint =>
        val shortName = shortRefNameViaImports(tp)
        if shortName != null then nameString(shortName)
        else super.toTextRef(tp)

      case _ => super.toTextRef(tp)
  }

  override def toText(tp: Type): Text = tp match
    case tp: TypeRef if !debugPrint =>
      val importedName = getImportedTypeName(tp.symbol)
      if importedName != null then nameString(importedName)
      else super.toText(tp)

    case _ => super.toText(tp)

  /** Override fullNameString to use shorter names based on imports.
   *  This is called by RefinedPrinter.toTextRef for packages.
   */
  override def fullNameString(sym: Symbol): String =
    if debugPrint then super.fullNameString(sym)
    else
      def findShortestPath(s: Symbol): String =
        if s.isRoot || s == NoSymbol || s.owner.isEffectiveRoot then nameString(s)
        else
          val owner = s.effectiveOwner
          if findImportedName(s, owner) != null then nameString(s)
          else findShortestPath(owner) + "." + nameString(s)

      findShortestPath(sym)

  override def nameString(name: Name): String =
    if (name.isReplAssignName) name.decode.toString.takeWhile(_ != '$')
    else super.nameString(name)

  override def toText(sym: Symbol): Text =
    if (sym.name.isReplAssignName) nameString(sym.name)
    else if (debugPrint) super.toText(sym)
    else keyString(sym) ~~ nameString(sym.name.stripModuleClassSuffix)

  inline private val qSc = '"';

  override def toText(const: Constant): Text =
    if (debugPrint) super.toText(const)
    else if (const.tag == Constants.StringTag) Str(s"${qSc}${const.value}$qSc")
    else Str(const.value.toString)

  override def dclText(sym: Symbol): Text = if (debugPrint) super.dclText(sym) else
    ("lazy": Text).provided(sym.is(Lazy)) ~~
    toText(sym) ~ {
      if (sym.is(Method)) {
        sym.info match {
          case tp: ExprType => ":" ~~ toText(tp.resType)
          case info => toText(info)
        }
      }
      else if (sym.isType && sym.info.isTypeAlias) toText(sym.info)
      else if (sym.isType || sym.isClass) ""
      else ":" ~~ toText(sym.info)
    }

  override def toTextSingleton(tp: SingletonType): Text =
    if (debugPrint)
      super.toTextSingleton(tp)
    else
      tp match {
        case ConstantType(const) => toText(const)
        case _                   => toTextRef(tp) ~ ".type"
      }

  // We don't want the colors coming from RefinedPrinter as the REPL uses its
  // own syntax coloring mechanism.
  override def coloredStr(text: String, color: String): String = text
  override def coloredText(text: Text, color: String): Text = text
}
