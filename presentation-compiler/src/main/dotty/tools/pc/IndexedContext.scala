package dotty.tools.pc

import scala.annotation.tailrec
import scala.util.control.NonFatal

import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.NameOps.moduleClassName
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.Scopes.EmptyScope
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.typer.ImportInfo
import dotty.tools.pc.IndexedContext.Result
import dotty.tools.pc.utils.MtagsEnrichments.*

sealed trait IndexedContext:
  given ctx: Context
  def scopeSymbols: List[Symbol]
  def names: IndexedContext.Names
  def rename(sym: Symbol): Option[String]
  def outer: IndexedContext

  def findSymbol(name: String): Option[List[Symbol]]

  final def findSymbol(name: Name): Option[List[Symbol]] =
    findSymbol(name.decoded)

  final def lookupSym(sym: Symbol): Result =
    findSymbol(sym.decodedName) match
      case Some(symbols) if symbols.exists(_ == sym) =>
        Result.InScope
      case Some(symbols)
          if symbols
            .exists(s => isTypeAliasOf(s, sym) || isTermAliasOf(s, sym)) =>
        Result.InScope
      // when all the conflicting symbols came from an old version of the file
      case Some(symbols) if symbols.nonEmpty && symbols.forall(_.isStale) =>
        Result.Missing
      case Some(_) => Result.Conflict
      case None => Result.Missing
  end lookupSym

  final def hasRename(sym: Symbol, as: String): Boolean =
    rename(sym) match
      case Some(v) => v == as
      case None => false

  // detects import scope aliases like
  // object Predef:
  //   val Nil = scala.collection.immutable.Nil
  private def isTermAliasOf(termAlias: Symbol, sym: Symbol): Boolean =
    termAlias.isTerm && (
      sym.info match
        case clz: ClassInfo => clz.appliedRef =:= termAlias.info.resultType
        case _ => false
    )

  private def isTypeAliasOf(alias: Symbol, sym: Symbol): Boolean =
    alias.isAliasType && alias.info.metalsDealias.typeSymbol == sym

  final def isEmpty: Boolean = this match
    case IndexedContext.Empty => true
    case _ => false

  final def importContext: IndexedContext =
    this match
      case IndexedContext.Empty => this
      case _ if ctx.owner.is(Package) => this
      case _ => outer.importContext

  @tailrec
  final def toplevelClashes(sym: Symbol): Boolean =
    if sym == NoSymbol || sym.owner == NoSymbol || sym.owner.isRoot then
      lookupSym(sym) match
        case IndexedContext.Result.Conflict => true
        case _ => false
    else toplevelClashes(sym.owner)

end IndexedContext

object IndexedContext:

  def apply(ctx: Context): IndexedContext =
    ctx match
      case NoContext => Empty
      case _ => LazyWrapper(using ctx)

  case object Empty extends IndexedContext:
    given ctx: Context = NoContext
    def findSymbol(name: String): Option[List[Symbol]] = None
    def scopeSymbols: List[Symbol] = List.empty
    val names: Names = Names(Map.empty, Map.empty)
    def rename(sym: Symbol): Option[String] = None
    def outer: IndexedContext = this

  class LazyWrapper(using val ctx: Context) extends IndexedContext:
    val outer: IndexedContext = IndexedContext(ctx.outer)
    val names: Names = extractNames(ctx)

    def findSymbol(name: String): Option[List[Symbol]] =
      names.symbols
        .get(name)
        .map(_.toList)
        .orElse(outer.findSymbol(name))

    def scopeSymbols: List[Symbol] =
      val acc = Set.newBuilder[Symbol]
      (this :: outers).foreach { ref =>
        acc ++= ref.names.symbols.values.flatten
      }
      acc.result.toList

    def rename(sym: Symbol): Option[String] =
      names.renames
        .get(sym)
        .orElse(outer.rename(sym))

    private def outers: List[IndexedContext] =
      val builder = List.newBuilder[IndexedContext]
      var curr = outer
      while !curr.isEmpty do
        builder += curr
        curr = curr.outer
      builder.result
  end LazyWrapper

  enum Result:
    case InScope, Conflict, Missing
    def exists: Boolean = this match
      case InScope | Conflict => true
      case Missing => false

  case class Names(
      symbols: Map[String, List[Symbol]],
      renames: Map[Symbol, String]
  )

  private def extractNames(ctx: Context): Names =
    def isAccessibleFromSafe(sym: Symbol, site: Type): Boolean =
      try sym.isAccessibleFrom(site, superAccess = false)
      catch
        case NonFatal(e) =>
          false

    def accessibleSymbols(site: Type, tpe: Type)(using
        Context
    ): List[Symbol] =
      tpe.decls.toList.filter(sym => isAccessibleFromSafe(sym, site))

    def accesibleMembers(site: Type)(using Context): List[Symbol] =
      site.allMembers
        .filter(denot =>
          try isAccessibleFromSafe(denot.symbol, site)
          catch
            case NonFatal(e) =>
              false
        )
        .map(_.symbol)
        .toList

    def allAccessibleSymbols(
        tpe: Type,
        filter: Symbol => Boolean = _ => true
    )(using Context): List[Symbol] =
      val initial = accessibleSymbols(tpe, tpe).filter(filter)
      val fromPackageObjects =
        initial
          .filter(_.isPackageObject)
          .flatMap(sym => accessibleSymbols(tpe, sym.thisType))
      initial ++ fromPackageObjects

    def fromImport(site: Type, name: Name)(using Context): List[Symbol] =
      List(
        site.member(name.toTypeName),
        site.member(name.toTermName),
        site.member(name.moduleClassName),
      )
        .flatMap(_.alternatives)
        .map(_.symbol)

    def fromImportInfo(
        imp: ImportInfo
    )(using Context): List[(Symbol, Option[TermName])] =
      val excludedNames = imp.excluded.map(_.decoded)

      if imp.isWildcardImport then
        allAccessibleSymbols(
          imp.site,
          sym => !excludedNames.contains(sym.name.decoded)
        ).map((_, None))
      else
        imp.forwardMapping.toList.flatMap { (name, rename) =>
          val isRename = name != rename
          if !isRename && !excludedNames.contains(name.decoded) then
            fromImport(imp.site, name).map((_, None))
          else if isRename then
            fromImport(imp.site, name).map((_, Some(rename)))
          else Nil
        }
      end if
    end fromImportInfo

    given Context = ctx
    val (symbols, renames) =
      if ctx.isImportContext then
        val (syms, renames) =
          fromImportInfo(ctx.importInfo.nn)
            .map((sym, rename) => (sym, rename.map(r => sym -> r.decoded)))
            .unzip
        (syms, renames.flatten.toMap)
      else if ctx.owner.isClass then
        val site = ctx.owner.thisType
        (accesibleMembers(site), Map.empty)
      else if ctx.scope != EmptyScope then (ctx.scope.toList, Map.empty)
      else (List.empty, Map.empty)

    val initial = Map.empty[String, List[Symbol]]
    val values =
      symbols.foldLeft(initial) { (acc, sym) =>
        val name = sym.decodedName
        val syms = acc.getOrElse(name, List.empty)
        acc.updated(name, sym :: syms)
      }
    Names(values, renames)
  end extractNames
end IndexedContext
