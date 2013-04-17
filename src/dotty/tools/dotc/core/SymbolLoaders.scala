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
import Contexts._, Symbols._, Flags._, SymDenotations._, Types._, Scopes._, util.Positions._, Names._
import StdNames._
import Decorators.StringDecorator
import pickling.ClassfileParser

object SymbolLoaders {
  /** A marker trait for a completer that replaces the original
   *  Symbol loader for an unpickled root.
   */
  trait SecondCompleter
}

/** A base class for Symbol loaders with some overridable behavior  */
class SymbolLoaders {

  protected def enterNew(
      owner: Symbol, member: Symbol,
      completer: SymbolLoader, scope: Scope = EmptyScope)(implicit ctx: Context): Symbol = {
    assert(scope.lookup(member.name) == NoSymbol, owner.fullName + "." + member.name)
    owner.asClass.enter(member, scope)
    member
  }

  /** Enter class with given `name` into scope of `owner`.
   */
  def enterClass(
      owner: Symbol, name: PreName, completer: SymbolLoader,
      flags: FlagSet = EmptyFlags, scope: Scope = EmptyScope)(implicit ctx: Context): Symbol = {
    val cls = ctx.newClassSymbol(owner, name.toTypeName, flags, completer, assocFile = completer.sourceFileOrNull)
    enterNew(owner, cls, completer, scope)
  }

  /** Enter module with given `name` into scope of `owner`.
   */
  def enterModule(
      owner: Symbol, name: PreName, completer: SymbolLoader,
      modFlags: FlagSet = EmptyFlags, clsFlags: FlagSet = EmptyFlags, scope: Scope = EmptyScope)(implicit ctx: CondensedContext): Symbol = {
    val module = ctx.newModuleSymbol(
      owner, name.toTermName, modFlags, clsFlags,
      (modul, _) => new ModuleClassCompleterWithDecls(modul, newScope, completer),
      assocFile = completer.sourceFileOrNull)
    enterNew(owner, module, completer, scope)
  }

  /** Enter package with given `name` into scope of `owner`
   *  and give them `completer` as type.
   */
  def enterPackage(owner: Symbol, pkg: ClassPath)(implicit ctx: CondensedContext): Symbol = {
    val pname = pkg.name.toTermName
    val preExisting = owner.info.decls lookup pname
    if (preExisting != NoSymbol) {
      // Some jars (often, obfuscated ones) include a package and
      // object with the same name. Rather than render them unusable,
      // offer a setting to resolve the conflict one way or the other.
      // This was motivated by the desire to use YourKit probes, which
      // require yjp.jar at runtime. See SI-2089.
      if (ctx.settings.termConflict.isDefault)
        throw new TypeError(
          s"""$owner contains object and package with same name: $pname
             |one of them needs to be removed from classpath""".stripMargin)
      else if (ctx.settings.termConflict.value == "package") {
        ctx.warning(
          s"Resolving package/object name conflict in favor of package ${preExisting.fullName}. The object will be inaccessible.")
        owner.asClass.delete(preExisting)
      } else {
        ctx.warning(
          s"Resolving package/object name conflict in favor of object ${preExisting.fullName}.  The package will be inaccessible.")
        return NoSymbol
      }
    }
    ctx.newModuleSymbol(owner, pname, PackageCreationFlags, PackageCreationFlags,
      (module, modcls) => new PackageLoader(module, pkg)).entered
  }

  /** Enter class and module with given `name` into scope of `owner`
   *  and give them `completer` as type.
   */
  def enterClassAndModule(
      owner: Symbol, name: PreName, completer: SymbolLoader,
      flags: FlagSet = EmptyFlags, scope: Scope = EmptyScope)(implicit ctx: CondensedContext) {
    val clazz = enterClass(owner, name, completer, flags, scope)
    val module = enterModule(
      owner, name, completer,
      modFlags = flags.toTermFlags & RetainedModuleValFlags,
      clsFlags = flags.toTypeFlags & RetainedModuleClassFlags,
      scope = scope)
  }

  /** In batch mode: Enter class and module with given `name` into scope of `owner`
   *  and give them a source completer for given `src` as type.
   *  In IDE mode: Find all toplevel definitions in `src` and enter then into scope of `owner`
   *  with source completer for given `src` as type.
   *  (overridden in interactive.Global).
   */
  def enterToplevelsFromSource(
      owner: Symbol, name: PreName, src: AbstractFile,
      scope: Scope = EmptyScope)(implicit ctx: CondensedContext) {
    enterClassAndModule(owner, name, new SourcefileLoader(src)(ctx.condensed), scope = scope)
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
  def initializeFromClassPath(owner: Symbol, classRep: ClassPath#ClassRep)(implicit ctx: CondensedContext) {
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
  class PackageLoader(override val sourceModule: TermSymbol, classpath: ClassPath)(implicit val cctx: CondensedContext)
      extends ClassCompleterWithDecls(newScope) with SymbolLoader { // !!! TODO: ClassCompleter needed?
    def description = "package loader " + classpath.name

    def doComplete(root: SymDenotation) {
      assert(root is PackageClass, root)
      val pre = root.owner.thisType
      root.info = ClassInfo(pre, root.symbol.asClass, Nil, root.decls, TermRef.withSym(pre, sourceModule))
      if (!sourceModule.isCompleted)
        sourceModule.completer.complete(sourceModule)
      if (!root.isRoot) {
        for (classRep <- classpath.classes) {
          initializeFromClassPath(root.symbol, classRep)
        }
      }
      if (!root.isEmptyPackage) {
        for (pkg <- classpath.packages) {
          enterPackage(root.symbol, pkg)
        }
        openPackageModule(root.symbol.asClass)
      }
    }
  }

  /** if there's a `package` member object in `pkgClass`, enter its members into it. */
  def openPackageModule(pkgClass: ClassSymbol)(implicit ctx: Context) {
    val pkgModule = pkgClass.info.decl(nme.PACKAGEkw).symbol
    if ((pkgModule is Module) &&
        (pkgModule.isCompleted ||
         !pkgModule.completer.isInstanceOf[SourcefileLoader]))
      // println("open "+pkgModule)//DEBUG
      openPackageModule(pkgModule, pkgClass)
  }

  def openPackageModule(container: Symbol, dest: ClassSymbol)(implicit ctx: Context) {
    def isImportable(sym: Symbol) = !(sym is Private) && !sym.isConstructor
    // unlink existing symbols in the package
    for (member <- container.info.decls.iterator) {
      if (isImportable(member)) {
        // todo: handle overlapping definitions in some way: mark as errors
        // or treat as abstractions. For now the symbol in the package module takes precedence.
        for (existing <- dest.info.decl(member.name).alternatives)
          dest.delete(existing.symbol)
      }
    }
    // enter non-private decls in the class
    for (member <- container.info.decls.iterator) {
      if (isImportable(member)) {
        dest.enter(member)
      }
    }
    // !!! TODO info.decls -> decls
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
 */
trait SymbolLoader extends LazyType {
  implicit val cctx: CondensedContext

  /** Load source or class file for `root`, return */
  def doComplete(root: SymDenotation): Unit

  def sourceFileOrNull: AbstractFile = null

  /** Description of the resource (ClassPath, AbstractFile, MsilFile)
   *  being processed by this loader
   */
  def description: String

  override def complete(root: SymDenotation): Unit = {
    def signalError(ex: Exception) {
      if (cctx.debug) ex.printStackTrace()
      val msg = ex.getMessage()
      cctx.error(
        if (msg eq null) "i/o error while loading " + root.name
        else "error while loading " + root.name + ",\n " + msg)
    }
    try {
      val start = currentTime
      if (cctx.settings.debugTrace.value)
        cctx.traceIndented(s">>>> loading ${root.debugString}", _ => s"<<<< loaded ${root.debugString}") {
          doComplete(root)
        }
      else
        doComplete(root)
      cctx.informTime("loaded " + description, start)
    } catch {
      case ex: IOException =>
        signalError(ex)
      case ex: Throwable =>
        println(s"exception caught when loading $root: $ex")
        throw ex
    } finally {
      def postProcess(denot: SymDenotation) =
        if (!denot.isCompleted &&
            !denot.completer.isInstanceOf[SymbolLoaders.SecondCompleter])
          denot.markAbsent()
      postProcess(root)
      if (!root.isRoot)
        postProcess(root.linkedClass.denot)
    }
  }
}

class ClassfileLoader(val classfile: AbstractFile)(implicit val cctx: CondensedContext) extends SymbolLoader {

  override def sourceFileOrNull: AbstractFile = classfile

  def description = "class file "+ classfile.toString

  def rootDenots(rootDenot: ClassDenotation): (ClassDenotation, ClassDenotation) = {
    val linkedDenot = rootDenot.linkedClass.denot match {
      case d: ClassDenotation => d
      case d =>
        // this can happen if the companion if shadowed by a val or type
        // in a package object; in this case, we make up some dummy denotation
        // as a stand in for loading.
        // An example for this situation is scala.reflect.Manifest, which exists
        // as a class in scala.reflect and as a val in scala.reflect.package.
        if (rootDenot is ModuleClass)
          cctx.newClassSymbol(
            rootDenot.owner, rootDenot.name.asTypeName, Synthetic,
              _ => NoType).classDenot
        else
          cctx.newModuleSymbol(
            rootDenot.owner, rootDenot.name.toTermName, Synthetic, Synthetic,
            (module, _) => new ModuleClassCompleterWithDecls(module, newScope))
            .moduleClass.denot.asClass
    }
    if (rootDenot is ModuleClass) (linkedDenot, rootDenot)
    else (rootDenot, linkedDenot)
  }

  def doComplete(root: SymDenotation) {
    val (classRoot, moduleRoot) = rootDenots(root.asClass)
    new ClassfileParser(classfile, classRoot, moduleRoot)(cctx).run()
  }
}

class SourcefileLoader(val srcfile: AbstractFile)(implicit val cctx: CondensedContext) extends SymbolLoader {
  def description = "source file "+ srcfile.toString
  override def sourceFileOrNull = srcfile
  def doComplete(root: SymDenotation): Unit = unsupported("doComplete")
}
