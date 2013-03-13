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

  protected def enterNew(owner: Symbol, member: Symbol, completer: SymbolLoader)
      (implicit ctx: Context): Symbol = {
    assert(owner.info.decls.lookup(member.name) == NoSymbol, owner.fullName + "." + member.name)
    owner.asClass.enter(member)
    member
  }

  /** Enter class with given `name` into scope of `owner`.
   */
  def enterClass(owner: Symbol, name: PreName, completer: SymbolLoader, flags: FlagSet = EmptyFlags)(implicit ctx: Context): Symbol = {
    val cls = ctx.newClassSymbol(owner, name.toTypeName, flags, completer, assocFile = completer.sourceFileOrNull)
    enterNew(owner, cls, completer)
  }

  /** Enter module with given `name` into scope of `owner`.
   */
  def enterModule(owner: Symbol, name: PreName, completer: SymbolLoader, modFlags: FlagSet = EmptyFlags, clsFlags: FlagSet = EmptyFlags)(implicit ctx: CondensedContext): Symbol = {
    def moduleCompleterFn(modul: TermSymbol, cls: ClassSymbol): LazyType =
      new ModuleClassCompleter(modul, completer)
    val module = ctx.newModuleSymbol(owner, name.toTermName, modFlags, clsFlags, moduleCompleterFn, assocFile = completer.sourceFileOrNull)
    enterNew(owner, module, completer)
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
    ctx.newModuleSymbol(owner, pname, PackageCreationFlags, PackageClassCreationFlags,
      (module, modcls) => new PackageLoader(module, pkg)).entered
  }

  /** Enter class and module with given `name` into scope of `owner`
   *  and give them `completer` as type.
   */
  def enterClassAndModule(owner: Symbol, name: PreName, completer: SymbolLoader, flags: FlagSet = EmptyFlags)(implicit ctx: CondensedContext) {
    val clazz = enterClass(owner, name, completer, flags)
    val module = enterModule(owner, name, completer, flags)
 /*
  * !!! disabled for now because it causes CyclicReference. Need to revisit
  *   if (!clazz.isAnonymousClass) {
      assert(clazz.companionModule == module, module)
      assert(module.companionClass == clazz, clazz)
    }
    */
  }

  /** In batch mode: Enter class and module with given `name` into scope of `owner`
   *  and give them a source completer for given `src` as type.
   *  In IDE mode: Find all toplevel definitions in `src` and enter then into scope of `owner`
   *  with source completer for given `src` as type.
   *  (overridden in interactive.Global).
   */
  def enterToplevelsFromSource(owner: Symbol, name: PreName, src: AbstractFile)(implicit ctx: CondensedContext) {
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
  class PackageLoader(module: TermSymbol, classpath: ClassPath)(implicit val cctx: CondensedContext)
      extends LazyModuleClassInfo(module) with SymbolLoader {
    def description = "package loader " + classpath.name

    def doComplete(root: SymDenotation) {
      assert(root is PackageClass, root)
      val pre = root.owner.thisType
      root.info = ClassInfo(pre, root.symbol.asClass, Nil, newScope, TermRef(pre, module))
      if (!module.isCompleted)
        module.completer.complete(module)
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
      // println("trying to complete "+root)  // !!! DEBUG
      val start = currentTime
      doComplete(root)
      cctx.informTime("loaded " + description, start)
    } catch {
      case ex: IOException =>
        signalError(ex)
      case ex: Throwable => // !!! DEBUG
        println("caught: "+ex)
        ex.printStackTrace()
        throw ex
    } finally {
      def postProcess(denot: SymDenotation) =
        if (!denot.isCompleted) denot.markAbsent()
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
      case d => throw new FatalError(s"linked class denot $d of $rootDenot is expected to be a ClassDenotation, but is a ${d.getClass}")
    }
    if (rootDenot is ModuleClass) (linkedDenot, rootDenot)
    else (rootDenot, linkedDenot)
  }

  def doComplete(root: SymDenotation) {
    val (classRoot, moduleRoot) = rootDenots(root.asClass)
    new ClassfileParser(classfile, classRoot, moduleRoot).run()
  }
}

class SourcefileLoader(val srcfile: AbstractFile)(implicit val cctx: CondensedContext) extends SymbolLoader {
  def description = "source file "+ srcfile.toString
  override def sourceFileOrNull = srcfile
  def doComplete(root: SymDenotation): Unit = unsupported("doComplete")
}

class ModuleClassCompleter(modul: TermSymbol, classCompleter: SymbolLoader)(implicit val cctx: CondensedContext)
  extends LazyModuleClassInfo(modul) with SymbolLoader {
  def description: String = classCompleter.description
  override def sourceFileOrNull = classCompleter.sourceFileOrNull
  def doComplete(root: SymDenotation): Unit = {
    classCompleter.doComplete(root)
  }
}
