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
import StdNames._
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
    val cls = ctx.newClassSymbol(owner, name.toTypeName, flags, completer, assocFile = completer.sourceFileOrNull)
    enterIfNew(owner, cls, completer)
  }

  /** Enter module with given `name` into scope of `owner`.
   */
  def enterModule(owner: Symbol, name: PreName, completer: SymbolLoader, flags: FlagSet = EmptyFlags)(implicit ctx: Context): Symbol = {
    val module = ctx.newModuleSymbol(owner, name.toTermName, flags, completer, assocFile = completer.sourceFileOrNull)
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
    ctx.newModuleSymbol(owner, pname, PackageCreationFlags, completer).entered
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
    enterClassAndModule(owner, name, new SourcefileLoader(src)(ctx.condensed))
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
  class PackageLoader(classpath: ClassPath)(implicit val cctx: CondensedContext) extends SymbolLoader {
    protected def description = "package loader " + classpath.name

    protected override def doComplete(root: SymDenotation) {
      assert(root.isPackageClass, root)
      root.info = ClassInfo(root.owner.thisType, root.symbol.asClass, Nil, newScope)
      if (!root.isRoot) {
        for (classRep <- classpath.classes) {
          initializeFromClassPath(root.symbol, classRep)
        }
      }
      if (!root.isEmptyPackage) {
        for (pkg <- classpath.packages) {
          enterPackage(root.symbol, pkg.name, new PackageLoader(pkg))
        }
        openPackageModule(root.symbol.asClass)
      }
    }
  }

  /** if there's a `package` member object in `pkgClass`, enter its members into it. */
  def openPackageModule(pkgClass: ClassSymbol)(implicit ctx: Context) {
    val pkgModule = pkgClass.info.decl(nme.PACKAGEkw).symbol
    if (pkgModule.isModule &&
        (pkgModule.isCompleted ||
         !pkgModule.completer.isInstanceOf[SourcefileLoader]))
      // println("open "+pkgModule)//DEBUG
      openPackageModule(pkgModule, pkgClass)
  }

  def openPackageModule(container: Symbol, dest: ClassSymbol)(implicit ctx: Context) {
    // unlink existing symbols in the package
    for (member <- container.info.decls.iterator) {
      if (!(member is Private) && !member.isConstructor) {
        // todo: handle overlapping definitions in some way: mark as errors
        // or treat as abstractions. For now the symbol in the package module takes precedence.
        for (existing <- dest.info.decl(member.name).alternatives)
          dest.delete(existing.symbol)
      }
    }
    // enter non-private decls in the class
    for (member <- container.info.decls.iterator) {
      if (!(member is Private) && !member.isConstructor) {
        dest.enter(member)
      }
    }
    // enter decls of parent classes
    for (p <- container.info.parents) {
      if (p.symbol != defn.ObjectClass) {
        openPackageModule(p.symbol, dest)
      }
    }
  }
}
/** A lazy type that completes itself by calling parameter doComplete.
 *  Any linked modules/classes or module classes are also initialized.
 *  Todo: consider factoring out behavior from TopClassCompleter/SymbolLoader into
 *  supertrait SymLoader
 */
abstract class SymbolLoader extends LazyType {
  implicit val cctx: CondensedContext

  /** Load source or class file for `root`, return */
  protected def doComplete(root: SymDenotation): Unit

  def sourceFileOrNull: AbstractFile = null

  /** Description of the resource (ClassPath, AbstractFile, MsilFile)
   *  being processed by this loader
   */
  protected def description: String

  override def complete(root: SymDenotation): Unit = {
    def signalError(ex: Exception) {
      if (cctx.settings.debug.value) ex.printStackTrace()
      val msg = ex.getMessage()
      cctx.error(
        if (msg eq null) "i/o error while loading " + root.name
        else "error while loading " + root.name + ", " + msg)
    }
    try {
      val start = currentTime
      doComplete(root)
      cctx.informTime("loaded " + description, start)
    } catch {
      case ex: IOException =>
      signalError(ex)
    } finally {
      def postProcess(denot: SymDenotation) =
        if (!denot.isCompleted) denot.markAbsent()
      postProcess(root)
      postProcess(root.linkedClass.denot)
    }
  }
}

class ClassfileLoader(val classfile: AbstractFile)(implicit val cctx: CondensedContext) extends SymbolLoader {

  override def sourceFileOrNull: AbstractFile = classfile

  protected def description = "class file "+ classfile.toString

  def rootDenots(rootDenot: ClassDenotation): (ClassDenotation, ClassDenotation) = {
    val linkedDenot = rootDenot.linkedClass.denot match {
      case d: ClassDenotation => d
      case d => throw new FatalError(s"linked class denot $d of $rootDenot is expected to be a ClassDenotation, but is a ${d.getClass}")
    }
    if (rootDenot.isModuleClass) (linkedDenot, rootDenot)
    else (rootDenot, linkedDenot)
  }

  protected def doComplete(root: SymDenotation) {
    val (classRoot, moduleRoot) = rootDenots(root.asClass)
    new ClassfileParser(classfile, classRoot, moduleRoot).run()
  }
}

class SourcefileLoader(val srcfile: AbstractFile)(implicit val cctx: CondensedContext) extends SymbolLoader {
  protected def description = "source file "+ srcfile.toString
  override def sourceFileOrNull = srcfile
  protected def doComplete(root: SymDenotation): Unit = unsupported("doComplete")
}
