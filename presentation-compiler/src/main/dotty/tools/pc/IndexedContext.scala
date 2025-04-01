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
  def rename(sym: Symbol): Option[Name]
  def findSymbol(name: Name): Option[List[Symbol]]

  final def lookupSym(sym: Symbol): Result =
    val isRelated = Set(sym.companion, sym)
    val res = findSymbol(sym.name) match
      case Some(symbols) if symbols.exists(isRelated) =>
        Result.InScope
      case Some(symbols)
          if symbols.exists(s => isNotConflictingWithDefault(s, sym) || isTypeAliasOf(s, sym) || isTermAliasOf(s, sym)) =>
            Result.InScope
      // when all the conflicting symbols came from an old version of the file
      case Some(symbols) if symbols.nonEmpty && symbols.forall(_.isStale) => Result.Missing
      case Some(symbols) if symbols.exists(!Interactive.isImportedByDefault(_)) && symbols.exists(rename(_).isEmpty) => Result.Conflict
      case _ if Interactive.isImportedByDefault(sym) => Result.InScope
      case _ => Result.Missing
    // if(sym.name.show == "None") Thread.currentThread().getStackTrace().foreach(println)
    // println(s"res: $sym $res ${ findSymbol(sym.name)}")
    res
  end lookupSym

  /**
   * Scala by default imports following packages:
   * https://scala-lang.org/files/archive/spec/3.4/02-identifiers-names-and-scopes.html
   * import java.lang.*
   * {
   *   import scala.*
   *   {
   *     import Predef.*
   *     { /* source */ }
   *   }
   * }
   *
   * This check is necessary for proper scope resolution, because when we compare symbols from
   * index including the underlying type like scala.collection.immutable.List it actually
   * is in current scope in form of type forwarder imported from Predef.
   */
  private def isNotConflictingWithDefault(sym: Symbol, queriedSym: Symbol): Boolean =
    sym.info.widenDealias =:= queriedSym.info.widenDealias && (Interactive.isImportedByDefault(sym))

  final def hasRename(sym: Symbol, as: Name): Boolean =
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

  private def isTypeAliasOf(alias: Symbol, queriedSym: Symbol): Boolean =
    alias.isAliasType && alias.info.deepDealias.typeSymbol  == queriedSym

  final def isEmpty: Boolean = this match
    case IndexedContext.Empty => true
    case _ => false

  @tailrec
  final def toplevelClashes(sym: Symbol): Boolean =
    if sym == NoSymbol || sym.owner == NoSymbol || sym.owner.isRoot then
      lookupSym(sym) match
        case IndexedContext.Result.Conflict => true
        case _ => false
    else toplevelClashes(sym.owner)

end IndexedContext

object IndexedContext:

  def apply(pos: SourcePosition)(using Context): IndexedContext =
    ctx match
      case NoContext => Empty
      case _ => LazyWrapper(pos)(using ctx)

  case object Empty extends IndexedContext:
    given ctx: Context = NoContext
    def findSymbol(name: Name): Option[List[Symbol]] = None
    def scopeSymbols: List[Symbol] = List.empty
    def rename(sym: Symbol): Option[Name] = None

  class LazyWrapper(pos: SourcePosition)(using val ctx: Context) extends IndexedContext:

    val completionContext = Completion.scopeContext(pos)
    val names: Map[String, Seq[SingleDenotation]] = completionContext.names.map{
      case (name, denotations) =>
        name.show -> denotations
    }
    val renames = completionContext.renames

    // def defaultScopes(name: Name): Option[List[Symbol]] =
    //   val fromPredef = defn.ScalaPredefModuleClass.membersNamed(name)
    //   val fromScala = defn.ScalaPackageClass.membersNamed(name)
    //   val fromJava = defn.JavaLangPackageClass.membersNamed(name)
    //   val predefList = if fromPredef.exists then List(fromPredef.first.symbol) else Nil
    //   val scalaList = if fromScala.exists then List(fromScala.first.symbol) else Nil
    //   val javaList = if fromJava.exists then List(fromJava.first.symbol) else Nil
    //   val combined = predefList ++ scalaList ++ javaList
    //   if combined.nonEmpty then Some(combined) else None

    def findSymbol(name: Name): Option[List[Symbol]] =

      names
        .get(name.show)
        .orElse(renames.get(name).flatMap(renamed => names.get(renamed.show)))
        .map(_.map(_.symbol).toList) //.orElse(defaultScopes(name))

    def scopeSymbols: List[Symbol] =
      names.values.flatten.map(_.symbol).toList

    def rename(sym: Symbol): Option[Name] =
      renames.get(sym.name)

  end LazyWrapper

  enum Result:
    case InScope, Conflict, Missing
    def exists: Boolean = this match
      case InScope | Conflict => true
      case Missing => false


end IndexedContext
