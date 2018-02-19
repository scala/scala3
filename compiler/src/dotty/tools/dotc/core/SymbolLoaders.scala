package dotty.tools
package dotc
package core

import java.io.{IOException, File}
import scala.compat.Platform.currentTime
import dotty.tools.io.{ ClassPath, ClassRepresentation, AbstractFile }
import classpath._
import Contexts._, Symbols._, Flags._, SymDenotations._, Types._, Scopes._, util.Positions._, Names._
import StdNames._, NameOps._
import Decorators.{PreNamedString, StringInterpolators}
import classfile.ClassfileParser
import util.Stats
import Decorators._
import scala.util.control.NonFatal
import ast.Trees._
import ast.tpd
import parsing.Parsers.OutlineParser
import reporting.trace

object SymbolLoaders {
  /** A marker trait for a completer that replaces the original
   *  Symbol loader for an unpickled root.
   */
  trait SecondCompleter
}

/** A base class for Symbol loaders with some overridable behavior  */
class SymbolLoaders {
  import ast.untpd._

  protected def enterNew(
      owner: Symbol, member: Symbol,
      completer: SymbolLoader, scope: Scope = EmptyScope)(implicit ctx: Context): Symbol = {
    val comesFromScan =
      completer.isInstanceOf[SourcefileLoader] && ctx.settings.scansource.value
    assert(comesFromScan || scope.lookup(member.name) == NoSymbol,
           s"${owner.fullName}.${member.name} already has a symbol")
    owner.asClass.enter(member, scope)
    member
  }

  /** Enter class with given `name` into scope of `owner`.
   */
  def enterClass(
      owner: Symbol, name: PreName, completer: SymbolLoader,
      flags: FlagSet = EmptyFlags, scope: Scope = EmptyScope)(implicit ctx: Context): Symbol = {
    val cls = ctx.newClassSymbol(owner, name.toTypeName.unmangleClassName.decode, flags, completer, assocFile = completer.sourceFileOrNull)
    enterNew(owner, cls, completer, scope)
  }

  /** Enter module with given `name` into scope of `owner`.
   */
  def enterModule(
      owner: Symbol, name: PreName, completer: SymbolLoader,
      modFlags: FlagSet = EmptyFlags, clsFlags: FlagSet = EmptyFlags, scope: Scope = EmptyScope)(implicit ctx: Context): Symbol = {
    val module = ctx.newModuleSymbol(
      owner, name.toTermName.decode, modFlags, clsFlags,
      (module, _) => completer.proxy withDecls newScope withSourceModule (_ => module),
      assocFile = completer.sourceFileOrNull)
    enterNew(owner, module, completer, scope)
    enterNew(owner, module.moduleClass, completer, scope)
  }

  /** Enter package with given `name` into scope of `owner`
   *  and give them `completer` as type.
   */
  def enterPackage(owner: Symbol, pname: TermName, completer: (TermSymbol, ClassSymbol) => PackageLoader)(implicit ctx: Context): Symbol = {
    val preExisting = owner.info.decls lookup pname
    if (preExisting != NoSymbol) {
      // Some jars (often, obfuscated ones) include a package and
      // object with the same name. Rather than render them unusable,
      // offer a setting to resolve the conflict one way or the other.
      // This was motivated by the desire to use YourKit probes, which
      // require yjp.jar at runtime. See SI-2089.
      if (ctx.settings.YtermConflict.isDefault)
        throw new TypeError(
          i"""$owner contains object and package with same name: $pname
             |one of them needs to be removed from classpath""")
      else if (ctx.settings.YtermConflict.value == "package") {
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
      completer).entered
  }

  /** Enter class and module with given `name` into scope of `owner`
   *  and give them `completer` as type.
   */
  def enterClassAndModule(
      owner: Symbol, name: PreName, completer: SymbolLoader,
      flags: FlagSet = EmptyFlags, scope: Scope = EmptyScope)(implicit ctx: Context): Unit = {
    val clazz = enterClass(owner, name, completer, flags, scope)
    val module = enterModule(
      owner, name, completer,
      modFlags = flags.toTermFlags & RetainedModuleValFlags,
      clsFlags = flags.toTypeFlags & RetainedModuleClassFlags,
      scope = scope)
  }

  /** If setting -scansource is set:
   *    Enter all toplevel classes and objects in file `src` into package `owner`, provided
   *    they are in the right package. Issue a warning if a class or object is in the wrong
   *    package, i.e. if the file path differs from the declared package clause.
   *  If -scansource is not set:
   *    Enter class and module with given `name` into scope of `owner`.
   *
   *  All entered symbols are given a source completer of `src` as info.
   */
  def enterToplevelsFromSource(
      owner: Symbol, name: PreName, src: AbstractFile,
      scope: Scope = EmptyScope)(implicit ctx: Context): Unit = {

    val completer = new SourcefileLoader(src)
    if (ctx.settings.scansource.value && ctx.run != null) {
      System.out.print(i"scanning $src ...")
      System.out.flush()
      if (src.exists && !src.isDirectory) {
        val filePath = owner.ownersIterator.takeWhile(!_.isRoot).map(_.name.toTermName).toList

        def addPrefix(pid: RefTree, path: List[TermName]): List[TermName] = pid match {
          case Ident(name: TermName) => name :: path
          case Select(qual: RefTree, name: TermName) => name :: addPrefix(qual, path)
          case _ => path
        }

        def enterScanned(unit: CompilationUnit)(implicit ctx: Context) = {

          def checkPathMatches(path: List[TermName], what: String, tree: MemberDef): Boolean = {
            val ok = filePath == path
            if (!ok)
              ctx.warning(i"""$what ${tree.name} is in the wrong directory.
                              |It was declared to be in package ${path.reverse.mkString(".")}
                              |But it is found in directory     ${filePath.reverse.mkString(File.separator)}""",
                          tree.pos)
            ok
          }

          def traverse(tree: Tree, path: List[TermName]): Unit = tree match {
            case PackageDef(pid, body) =>
              val path1 = addPrefix(pid, path)
              for (stat <- body) traverse(stat, path1)
            case tree: TypeDef if tree.isClassDef =>
              if (checkPathMatches(path, "class", tree))
                enterClassAndModule(owner, tree.name, completer, scope = scope)
                  // It might be a case class or implicit class,
                  // so enter class and module to be on the safe side
            case tree: ModuleDef =>
              if (checkPathMatches(path, "object", tree))
                enterModule(owner, tree.name, completer, scope = scope)
            case _ =>
          }

          traverse(new OutlineParser(unit.source).parse(), Nil)
        }

        val unit = new CompilationUnit(ctx.run.getSource(src.path))
        enterScanned(unit)(ctx.run.runContext.fresh.setCompilationUnit(unit))
        System.out.println(" done")
      }
    }
    else enterClassAndModule(owner, name, completer, scope = scope)
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
      (owner.fullName.toString == "scala" || owner.fullName.toString == "scala.reflect")

  /** Initialize toplevel class and module symbols in `owner` from class path representation `classRep`
   */
  def initializeFromClassPath(owner: Symbol, classRep: ClassRepresentation)(implicit ctx: Context): Unit = {
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
  class PackageLoader(_sourceModule: TermSymbol, classPath: ClassPath)
      extends SymbolLoader {
    override def sourceModule(implicit ctx: Context) = _sourceModule
    def description(implicit ctx: Context) = "package loader " + sourceModule.fullName

    private[this] var enterFlatClasses: Option[Context => Unit] = None

    Stats.record("package scopes")

    /** The scope of a package. This is different from a normal scope
  	 *  in two aspects:
	   *
	   *   1. Names of scope entries are kept in mangled form.
	   *   2. Some function types in the `scala` package are synthesized.
  	 */
    final class PackageScope extends MutableScope {
      override def newScopeEntry(name: Name, sym: Symbol)(implicit ctx: Context): ScopeEntry =
        super.newScopeEntry(name.mangled, sym)

      override def lookupEntry(name: Name)(implicit ctx: Context): ScopeEntry = {
        val mangled = name.mangled
        val e = super.lookupEntry(mangled)
        if (e != null) e
        else if (isFlatName(mangled.toSimpleName) && enterFlatClasses.isDefined) {
          Stats.record("package scopes with flatnames entered")
          enterFlatClasses.get(ctx)
          lookupEntry(name)
        }
        else e
      }

      override def ensureComplete()(implicit ctx: Context) =
        for (enter <- enterFlatClasses) enter(ctx)

      override def newScopeLikeThis() = new PackageScope
    }

    private[core] val currentDecls: MutableScope = new PackageScope()

    def isFlatName(name: SimpleName) = name.lastIndexOf('$', name.length - 2) >= 0

    def isFlatName(classRep: ClassRepresentation) = {
      val idx = classRep.name.indexOf('$')
      idx >= 0 && idx < classRep.name.length - 1
    }

    def maybeModuleClass(classRep: ClassRepresentation) = classRep.name.last == '$'

    private def enterClasses(root: SymDenotation, packageName: String, flat: Boolean)(implicit ctx: Context) = {
      def isAbsent(classRep: ClassRepresentation) =
        !root.unforcedDecls.lookup(classRep.name.toTypeName).exists

      if (!root.isRoot) {
        val classReps = classPath.list(packageName).classesAndSources

        for (classRep <- classReps)
          if (!maybeModuleClass(classRep) && isFlatName(classRep) == flat &&
            (!flat || isAbsent(classRep))) // on 2nd enter of flat names, check that the name has not been entered before
            initializeFromClassPath(root.symbol, classRep)
        for (classRep <- classReps)
          if (maybeModuleClass(classRep) && isFlatName(classRep) == flat &&
              isAbsent(classRep))
            initializeFromClassPath(root.symbol, classRep)
      }
    }

    def doComplete(root: SymDenotation)(implicit ctx: Context): Unit = {
      assert(root is PackageClass, root)
      val pre = root.owner.thisType
      root.info = ClassInfo(pre, root.symbol.asClass, Nil, currentDecls, pre select sourceModule)
      if (!sourceModule.isCompleted)
        sourceModule.completer.complete(sourceModule)

      val packageName = if (root.isEffectiveRoot) "" else root.fullName.mangledString

      enterFlatClasses = Some { ctx =>
        enterFlatClasses = None
        enterClasses(root, packageName, flat = true)(ctx)
      }
      enterClasses(root, packageName, flat = false)
      if (!root.isEmptyPackage)
        for (pkg <- classPath.packages(packageName)) {
          val fullName = pkg.name
          val name =
            if (packageName.isEmpty) fullName
            else fullName.substring(packageName.length + 1)

          enterPackage(root.symbol, name.toTermName,
            (module, modcls) => new PackageLoader(module, classPath))
        }
    }
  }
}

/** A lazy type that completes itself by calling parameter doComplete.
 *  Any linked modules/classes or module classes are also initialized.
 */
abstract class SymbolLoader extends LazyType {

  /** Load source or class file for `root`, return */
  def doComplete(root: SymDenotation)(implicit ctx: Context): Unit

  def sourceFileOrNull: AbstractFile = null

  /** Description of the resource (ClassPath, AbstractFile)
   *  being processed by this loader
   */
  def description(implicit ctx: Context): String

  override def complete(root: SymDenotation)(implicit ctx: Context): Unit = {
    def signalError(ex: Exception): Unit = {
      if (ctx.debug) ex.printStackTrace()
      val msg = ex.getMessage()
      ctx.error(
        if (msg eq null) "i/o error while loading " + root.name
        else "error while loading " + root.name + ",\n" + msg)
    }
    try {
      val start = currentTime
      if (ctx.settings.YdebugTrace.value)
        trace(s">>>> loading ${root.debugString}", _ => s"<<<< loaded ${root.debugString}") {
          doComplete(root)
        }
      else
        doComplete(root)
      ctx.informTime("loaded " + description, start)
    } catch {
      case ex: IOException =>
        signalError(ex)
      case NonFatal(ex) =>
        println(s"exception caught when loading $root: $ex")
        throw ex
    } finally {
      def postProcess(denot: SymDenotation) =
        if (!denot.isCompleted &&
            !denot.completer.isInstanceOf[SymbolLoaders.SecondCompleter])
          denot.markAbsent()
      postProcess(root)
      if (!root.isRoot)
        postProcess(root.scalacLinkedClass.denot)
    }
  }

  protected def rootDenots(rootDenot: ClassDenotation)(implicit ctx: Context): (ClassDenotation, ClassDenotation) = {
    val linkedDenot = rootDenot.scalacLinkedClass.denot match {
      case d: ClassDenotation => d
      case d =>
        // this can happen if the companion if shadowed by a val or type
        // in a package object; in this case, we make up some dummy denotation
        // as a stand in for loading.
        // An example for this situation is scala.reflect.Manifest, which exists
        // as a class in scala.reflect and as a val in scala.reflect.package.
        if (rootDenot is ModuleClass)
          ctx.newClassSymbol(
            rootDenot.owner, rootDenot.name.stripModuleClassSuffix.asTypeName, Synthetic,
              _ => NoType).classDenot
        else
          ctx.newModuleSymbol(
            rootDenot.owner, rootDenot.name.toTermName, Synthetic, Synthetic,
            (module, _) => new NoCompleter() withDecls newScope withSourceModule (_ => module))
            .moduleClass.denot.asClass
    }
    if (rootDenot is ModuleClass) (linkedDenot, rootDenot)
    else (rootDenot, linkedDenot)
  }
}

class ClassfileLoader(val classfile: AbstractFile) extends SymbolLoader {

  override def sourceFileOrNull: AbstractFile = classfile

  def description(implicit ctx: Context) = "class file " + classfile.toString

  override def doComplete(root: SymDenotation)(implicit ctx: Context): Unit =
    load(root)

  def load(root: SymDenotation)(implicit ctx: Context): Unit = {
    val (classRoot, moduleRoot) = rootDenots(root.asClass)
    val classfileParser = new ClassfileParser(classfile, classRoot, moduleRoot)(ctx)
    val result = classfileParser.run()
    if (mayLoadTreesFromTasty) {
      result match {
        case Some(unpickler: tasty.DottyUnpickler) =>
          classRoot.classSymbol.treeOrProvider = unpickler
          moduleRoot.classSymbol.treeOrProvider = unpickler
        case _ =>
      }
    }
  }

  private def mayLoadTreesFromTasty(implicit ctx: Context): Boolean =
    ctx.settings.YretainTrees.value || ctx.settings.Xlink.value || ctx.settings.fromTasty.value
}

class SourcefileLoader(val srcfile: AbstractFile) extends SymbolLoader {
  def description(implicit ctx: Context) = "source file " + srcfile.toString
  override def sourceFileOrNull = srcfile
  def doComplete(root: SymDenotation)(implicit ctx: Context): Unit =
    ctx.run.lateCompile(srcfile, typeCheck = ctx.settings.YretainTrees.value)
}
