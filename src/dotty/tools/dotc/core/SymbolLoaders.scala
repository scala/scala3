/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Martin Odersky
 */

package dotty.tools
package dotc
package core

import java.io.IOException
import scala.compat.Platform.currentTime
import dotty.tools.io.{ ClassPath, AbstractFile }
import Contexts._, Symbols._, Flags._, SymDenotations._, Types._, Scopes._, Positions._, Names._
import Decorators.StringDecorator
import pickling.ClassfileParser

/** A base class for Symbol loaders with some overridable behavior  */
class SymbolLoaders {

  protected def enterIfNew(owner: Symbol, member: Symbol, completer: SymbolLoader)(implicit ctx: Context): Symbol = {
    assert(owner.info.decls.lookup(member.name) == NoSymbol, owner.fullName + "." + member.name)
    owner.info.decls enter member
    member
  }

  /** Enter class with given `name` into scope of `owner`.
   */
  def enterClass(owner: Symbol, name: PreName, completer: SymbolLoader, flags: FlagSet = EmptyFlags)(implicit ctx: Context): Symbol = {
    val cls = ctx.newLazyClassSymbol(owner, name.toTypeName, flags, completer, assocFile = completer.sourceFileOrNull)
    enterIfNew(owner, cls, completer)
  }

  /** Enter module with given `name` into scope of `owner`.
   */
  def enterModule(owner: Symbol, name: PreName, completer: SymbolLoader, flags: FlagSet = EmptyFlags)(implicit ctx: Context): Symbol = {
    val module = ctx.newLazyModuleSymbols(owner, name.toTermName, flags, completer, assocFile = completer.sourceFileOrNull)._1
    enterIfNew(owner, module, completer)
  }

  /** Enter package with given `name` into scope of `owner`
   *  and give them `completer` as type.
   */
  def enterPackage(owner: Symbol, name: PreName, completer: SymbolLoader)(implicit ctx: Context): Symbol = {
    val pname = name.toTermName
    val preExisting = owner.info.decls lookup pname
    if (preExisting != NoSymbol) {
      // Some jars (often, obfuscated ones) include a package and
      // object with the same name. Rather than render them unusable,
      // offer a setting to resolve the conflict one way or the other.
      // This was motivated by the desire to use YourKit probes, which
      // require yjp.jar at runtime. See SI-2089.
      if (ctx.settings.termConflict.isDefault)
        throw new TypeError(
          s"""$owner contains object and package with same name: $name
             |one of them needs to be removed from classpath""".stripMargin)
      else if (ctx.settings.termConflict.value == "package") {
        ctx.warning(
          s"Resolving package/object name conflict in favor of package ${preExisting.fullName}. The object will be inaccessible.")
        owner.info.decls.unlink(preExisting)
      } else {
        ctx.warning(
          s"Resolving package/object name conflict in favor of object ${preExisting.fullName}.  The package will be inaccessible.")
        return NoSymbol
      }
    }
    ctx.newLazyModuleSymbols(owner, pname, PackageCreationFlags, completer)._1.entered
  }

  /** Enter class and module with given `name` into scope of `owner`
   *  and give them `completer` as type.
   */
  def enterClassAndModule(owner: Symbol, name: PreName, completer: SymbolLoader, flags: FlagSet = EmptyFlags)(implicit ctx: Context) {
    val clazz = enterClass(owner, name, completer, flags)
    val module = enterModule(owner, name, completer, flags)
    if (!clazz.isAnonymousClass) {
      assert(clazz.companionModule == module, module)
      assert(module.companionClass == clazz, clazz)
    }
  }

  /** In batch mode: Enter class and module with given `name` into scope of `owner`
   *  and give them a source completer for given `src` as type.
   *  In IDE mode: Find all toplevel definitions in `src` and enter then into scope of `owner`
   *  with source completer for given `src` as type.
   *  (overridden in interactive.Global).
   */
  def enterToplevelsFromSource(owner: Symbol, name: PreName, src: AbstractFile)(implicit ctx: Context) {
    ??? // !!! enterClassAndModule(owner, name, new SourcefileLoader(src))
  }

  /** The package objects of scala and scala.reflect should always
   *  be loaded in binary if classfiles are available, even if sourcefiles
   *  are newer. Late-compiling these objects from source leads to compilation
   *  order issues.
   *  Note: We do a name-base comparison here because the method is called before we even
   *  have ReflectPackage defined.
   */
  def binaryOnly(owner: Symbol, name: String)(implicit ctx: Context): Boolean =
    name == "package" &&
      (owner.fullName == "scala" || owner.fullName == "scala.reflect")

  /** Initialize toplevel class and module symbols in `owner` from class path representation `classRep`
   */
  def initializeFromClassPath(owner: Symbol, classRep: ClassPath#ClassRep)(implicit ctx: Context) {
    ((classRep.binary, classRep.source): @unchecked) match {
      case (Some(bin), Some(src)) if needCompile(bin, src) && !binaryOnly(owner, classRep.name) =>
        if (ctx.settings.verbose.value) ctx.inform("[symloader] picked up newer source file for " + src.path)
        enterToplevelsFromSource(owner, classRep.name, src)
      case (None, Some(src)) =>
        if (ctx.settings.verbose.value) ctx.inform("[symloader] no class, picked up source file for " + src.path)
        enterToplevelsFromSource(owner, classRep.name, src)
      case (Some(bin), _) =>
        enterClassAndModule(owner, classRep.name, ctx.platform.newClassLoader(bin))
    }
  }

  def needCompile(bin: AbstractFile, src: AbstractFile) =
    src.lastModified >= bin.lastModified

  /** Load contents of a package
   */
  class PackageLoader(classpath: ClassPath)(cctx: CondensedContext) extends SymbolLoader {
    implicit val ctx: Context = cctx
    protected def description = "package loader " + classpath.name

    protected def doLoad(root: LazyClassDenotation) = doComplete(root)

    protected def doComplete(root: LazyClassDenotation) {
      assert(root.isPackageClass, root)
      root.parents = Nil
      root.decls = newScope
      if (!root.isRoot) {
        for (classRep <- classpath.classes) {
          initializeFromClassPath(root.symbol, classRep)
        }
      }
      if (!root.isEmptyPackage) {
        for (pkg <- classpath.packages) {
          enterPackage(root.symbol, pkg.name, new PackageLoader(pkg)(cctx))
        }

        openPackageModule(root.symbol)
      }
    }
  }

  def openPackageModule(pkgClass: Symbol): Unit = ???
}
/** A lazy type that completes itself by calling parameter doComplete.
 *  Any linked modules/classes or module classes are also initialized.
 *  Todo: consider factoring out behavior from TopClassCompleter/SymbolLoader into
 *  supertrait SymLoader
 */
abstract class SymbolLoader extends ClassCompleter {
  implicit val ctx: Context

  /** Load source or class file for `root`, return */
  protected def doComplete(root: LazyClassDenotation): Unit

  def sourceFileOrNull: AbstractFile = null

  /** Description of the resource (ClassPath, AbstractFile, MsilFile)
   *  being processed by this loader
   */
  protected def description: String

  override def complete(root: LazyClassDenotation) = {
    def signalError(ex: Exception) {
      if (ctx.settings.debug.value) ex.printStackTrace()
      val msg = ex.getMessage()
      ctx.error(
        if (msg eq null) "i/o error while loading " + root.name
        else "error while loading " + root.name + ", " + msg)
    }
    try {
      val start = currentTime
      doComplete(root)
      ctx.informTime("loaded " + description, start)
    } catch {
      case ex: IOException =>
        signalError(ex)
      case ex: MissingRequirementError =>
        signalError(ex)
    } finally {
      root.linkedClass.denot match {
        case companion: LazyClassDenotation => companion.completer = null
      }
    }
  }
}

class ClassfileLoader(val classfile: AbstractFile)(cctx: CondensedContext) extends SymbolLoader {
  implicit val ctx: Context = cctx

  override def sourceFileOrNull: AbstractFile = classfile

  protected def description = "class file "+ classfile.toString

  def rootDenots(rootDenot: LazyClassDenotation): (LazyClassDenotation, LazyClassDenotation) = {
    val linkedDenot = rootDenot.linkedClass.denot match {
      case d: LazyClassDenotation => d
      case d => throw new FatalError(s"linked class denot $d of $rootDenot is expected to be a LazyClassDenot, but is a ${d.getClass}")
    }
    if (rootDenot.isModule) (linkedDenot, rootDenot)
    else (rootDenot, linkedDenot)
  }

  protected def doComplete(root: LazyClassDenotation) {
    val (classRoot, moduleRoot) = rootDenots(root)
    new ClassfileParser(classfile, classRoot, moduleRoot)(cctx).run()
  }
}
/*
  class MsilFileLoader(msilFile: MsilFile) extends SymbolLoader with FlagAssigningCompleter {
    private def typ = msilFile.msilType
    private object typeParser extends clr.TypeParser {
      val global: SymbolLoaders.this.global.type = SymbolLoaders.this.global
    }

    protected def description = "MsilFile "+ typ.FullName + ", assembly "+ typ.Assembly.FullName
    protected def doComplete(root: Symbol) { typeParser.parse(typ, root) }
  }

  class SourcefileLoader(val srcfile: AbstractFile) extends SymbolLoader with FlagAssigningCompleter {
    protected def description = "source file "+ srcfile.toString
    override def fromSource = true
    override def sourcefile = Some(srcfile)
    protected def doComplete(root: Symbol): Unit = global.currentRun.compileLate(srcfile)
  }

  object moduleClassLoader extends SymbolLoader with FlagAssigningCompleter {
    protected def description = "module class loader"
    protected def doComplete(root: Symbol) { root.sourceModule.initialize }
  }

  object clrTypes extends clr.CLRTypes {
    val global: SymbolLoaders.this.global.type = SymbolLoaders.this.global
    if (global.forMSIL) init()
  }

  /** used from classfile parser to avoid cyclies */
  var parentsLevel = 0
  var pendingLoadActions: List[() => Unit] = Nil
}

object SymbolLoadersStats {
  import scala.reflect.internal.TypesStats.typerNanos
  val classReadNanos = Statistics.newSubTimer  ("time classfilereading", typerNanos)
}
*/