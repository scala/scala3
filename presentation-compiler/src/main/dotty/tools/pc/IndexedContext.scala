package dotty.tools.pc

import scala.annotation.tailrec
import scala.util.control.NonFatal

import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Denotations.PreDenotation
import dotty.tools.dotc.core.Denotations.SingleDenotation
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.NameOps.*
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.Scopes.EmptyScope
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.interactive.Completion
import dotty.tools.dotc.interactive.Interactive
import dotty.tools.dotc.typer.ImportInfo
import dotty.tools.dotc.util.SourcePosition
import dotty.tools.pc.IndexedContext.Result
import dotty.tools.pc.utils.InteractiveEnrichments.*

sealed trait IndexedContext:
  given ctx: Context
  def scopeSymbols: List[Symbol]
  def rename(sym: Symbol): Option[String]
  def findSymbol(name: Name, fromPrefix: Option[Type] = None): Option[List[Symbol]]
  def findSymbolInLocalScope(name: String): Option[List[Symbol]]

  final def lookupSym(sym: Symbol, fromPrefix: Option[Type] = None): Result =
    def all(symbol: Symbol): Set[Symbol] =
      Set(symbol, symbol.companionModule, symbol.companionClass, symbol.companion).filter(_ != NoSymbol)
    val isRelated = all(sym) ++ all(sym.dealiasType)
    findSymbol(sym.name, fromPrefix) match
      case Some(symbols) if symbols.exists(isRelated) => Result.InScope
      case Some(symbols) if symbols.exists(isTermAliasOf(_, sym)) => Result.InScope
      case Some(symbols) if symbols.map(_.dealiasType).exists(isRelated) => Result.InScope
      case Some(symbols) if symbols.nonEmpty && symbols.forall(_.isStale) => Result.Missing
      case Some(symbols) if symbols.exists(rename(_).isEmpty) => Result.Conflict
      case Some(symbols) => Result.InScope
      case _ => Result.Missing

  final def hasRename(sym: Symbol, as: String): Boolean =
    rename(sym) match
      case Some(v) =>
        v == as
      case None => false

  // detects import scope aliases like
  // object Predef:
  //   val Nil = scala.collection.immutable.Nil
  private def isTermAliasOf(termAlias: Symbol, queriedSym: Symbol): Boolean =
    termAlias.isTerm && (
      queriedSym.info match
        case clz: ClassInfo => clz.appliedRef =:= termAlias.info.resultType
        case _ => false
    )

  @tailrec
  final def toplevelClashes(sym: Symbol, inImportScope: Boolean): Boolean =
    if sym == NoSymbol || sym.owner == NoSymbol || sym.owner.isRoot then
      val possibleConflictingSymbols = findSymbolInLocalScope(sym.name.show)
      // if it's import scope we only care about toplevel conflicts, not any clashes inside objects etc.
      val symbolClashes = if inImportScope then
        // It's toplevel if it's parent is a package
        possibleConflictingSymbols.filter(_.exists(_.owner.is(Package)))
      else
        possibleConflictingSymbols
      symbolClashes match
        case Some(symbols) if !symbols.contains(sym) => true
        case _ => false
    else toplevelClashes(sym.owner, inImportScope)

end IndexedContext

object IndexedContext:

  def apply(pos: SourcePosition)(using Context): IndexedContext =
    ctx match
      case NoContext => Empty
      case _ => LazyWrapper(pos)(using ctx)

  case object Empty extends IndexedContext:
    given ctx: Context = NoContext
    def findSymbol(name: Name, fromPrefix: Option[Type]): Option[List[Symbol]] = None
    def findSymbolInLocalScope(name: String): Option[List[Symbol]] = None
    def scopeSymbols: List[Symbol] = List.empty
    def rename(sym: Symbol): Option[String] = None

  class LazyWrapper(pos: SourcePosition)(using val ctx: Context) extends IndexedContext:

    val completionContext = Completion.scopeContext(pos)
    val names: Map[String, Seq[SingleDenotation]] = completionContext.names.toList.groupBy(_._1.show).map {
      case (name, denotations) =>
        val denots = denotations.flatMap(_._2)
        val nonRoot = denots.filter(!_.symbol.owner.isRoot)
        val (importedByDefault, conflictingValue) =
          denots.partition(denot => Interactive.isImportedByDefault(denot.symbol))
        if importedByDefault.nonEmpty && conflictingValue.nonEmpty then
          name.trim -> conflictingValue
        else
          name.trim -> nonRoot
    }
    val renames = completionContext.renames

    def defaultScopes(name: Name): Option[List[Symbol]] =
      List(defn.ScalaPredefModuleClass, defn.ScalaPackageClass, defn.JavaLangPackageClass)
        .map(_.membersNamed(name))
        .collect { case denot if denot.exists => denot.first.symbol }
        .toList match
        case Nil => None
        case list => Some(list)

    override def findSymbolInLocalScope(name: String): Option[List[Symbol]] =
      names.get(name).map(_.map(_.symbol).toList).filter(_.nonEmpty)
    def findSymbol(name: Name, fromPrefix: Option[Type]): Option[List[Symbol]] =
      names
        .get(name.show)
        .map { denots =>
          def skipThisType(tp: Type): Type = tp match
            case ThisType(prefix) => skipThisType(prefix)
            case _ => tp

          val filteredDenots = fromPrefix match
            case Some(prefix) =>
              val target = skipThisType(prefix)
              denots.filter { denot =>
                denot.prefix == NoPrefix ||
                (denot.prefix match
                  case tref: TermRef =>
                    tref.termSymbol.info <:< target
                  case otherPrefix =>
                    otherPrefix <:< target)
              }
            case None => denots

          filteredDenots.map(_.symbol).toList
        }
        .orElse(defaultScopes(name)).filter(_.nonEmpty)

    def scopeSymbols: List[Symbol] =
      names.values.flatten.map(_.symbol).toList

    def rename(sym: Symbol): Option[String] =
      renames.get(sym).orElse(renames.get(sym.companion)).map(_.decoded)

  end LazyWrapper

  enum Result:
    case InScope, Conflict, Missing
    def exists: Boolean = this match
      case InScope | Conflict => true
      case Missing => false

end IndexedContext
