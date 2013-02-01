/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Martin Odersky
 */

package dotty.tools
package dotc
package core

import java.io.IOException
import scala.compat.Platform.currentTime
import dotty.tools.io.{ClassPath, AbstractFile}
import Contexts._, Symbols._, Flags._, SymDenotations._, Types._
import Decorators.StringDecorator
//import classfile.ClassfileParser

abstract class SymbolLoader extends ClassCompleter


class SymbolLoaders {

  protected def enterIfNew(owner: Symbol, member: Symbol, completer: SymbolLoader)(implicit ctx: Context): Symbol = {
    assert(owner.info.decls.lookup(member.name) == NoSymbol, owner.fullName + "." + member.name)
    owner.info.decls enter member
    member
  }

  /** Enter class with given `name` into scope of `owner`.
   */
  def enterClass(owner: Symbol, name: String, completer: SymbolLoader)(implicit ctx: Context): Symbol = {
    val cls = ctx.newLazyClassSymbol(owner, name.toTypeName, EmptyFlags, completer)
    enterIfNew(owner, cls, completer)
  }

  /** Enter module with given `name` into scope of `owner`.
   */
  def enterModule(owner: Symbol, name: String, completer: SymbolLoader)(implicit ctx: Context): Symbol = {
    val module = ctx.newLazyModuleSymbol(owner, name.toTermName, EmptyFlags, completer)
    enterIfNew(owner, module, completer)
  }

  /** Enter package with given `name` into scope of `owner`
   *  and give them `completer` as type.
   */
  def enterPackage(owner: Symbol, name: String, completer: SymbolLoader)(implicit ctx: Context): Symbol = {
    val pname = name.toTermName
    val preExisting = owner.info.decls lookup pname
    if (preExisting != NoSymbol) {
      // Some jars (often, obfuscated ones) include a package and
      // object with the same name. Rather than render them unusable,
      // offer a setting to resolve the conflict one way or the other.
      // This was motivated by the desire to use YourKit probes, which
      // require yjp.jar at runtime. See SI-2089.
      if (ctx.settings.YtermConflict == ctx.settings.default.YtermConflict)
        throw new TypeError(
          s"""$owner contains object and package with same name: $name
             |one of them needs to be removed from classpath""".stripMargin)
      else if (ctx.settings.YtermConflict.value == "package") {
        ctx.warning(
          s"Resolving package/object name conflict in favor of package ${preExisting.fullName}. The object will be inaccessible.")
        owner.info.decls.unlink(preExisting)
      } else {
        ctx.warning(
          s"Resolving package/object name conflict in favor of object ${preExisting.fullName}.  The package will be inaccessible.")
        return NoSymbol
      }
    }
    ctx.newLazyModuleSymbol(owner, pname, PackageCreationFlags, completer).entered
  }

  /** Enter class and module with given `name` into scope of `owner`
   *  and give them `completer` as type.
   */
  def enterClassAndModule(owner: Symbol, name: String, completer: SymbolLoader)(implicit ctx: Context) {
    val clazz = enterClass(owner, name, completer)
    val module = enterModule(owner, name, completer)
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
  def enterToplevelsFromSource(owner: Symbol, name: String, src: AbstractFile)(implicit ctx: Context) {
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
    ((classRep.binary, classRep.source) : @unchecked) match {
      case (Some(bin), Some(src))
      if needCompile(bin, src) && !binaryOnly(owner, classRep.name) =>
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
}/*
  /**
   * A lazy type that completes itself by calling parameter doComplete.
   * Any linked modules/classes or module classes are also initialized.
   * Todo: consider factoring out behavior from TopClassCompleter/SymbolLoader into
   * supertrait SymLoader
   */
  abstract class SymbolLoader extends ClassCompleter {

    /** Load source or class file for `root`, return */
    protected def doComplete(root: Symbol): Unit

    def sourcefile: Option[AbstractFile] = None

    /**
     * Description of the resource (ClassPath, AbstractFile, MsilFile)
     * being processed by this loader
     */
    protected def description: String

    private var ok = false

    private def setSource(sym: Symbol) {
      sourcefile foreach (sf => sym match {
        case cls: ClassSymbol => cls.sourceFile = sf
        case mod: ModuleSymbol => mod.moduleClass.sourceFile = sf
        case _ => ()
      })
    }

    override def complete(root: Symbol) {
      def signalError(ex: Exception) {
        ok = false
        if (settings.debug.value) ex.printStackTrace()
        val msg = ex.getMessage()
        // SI-5593 Scaladoc's current strategy is to visit all packages in search of user code that can be documented
        // therefore, it will rummage through the classpath triggering errors whenever it encounters package objects
        // that are not in their correct place (see bug for details)
        if (!settings.isScaladoc)
          globalError(
            if (msg eq null) "i/o error while loading " + root.name
            else "error while loading " + root.name + ", " + msg);
      }
      try {
        val start = currentTime
        val currentphase = phase
        doComplete(root)
        phase = currentphase
        informTime("loaded " + description, start)
        ok = true
        setSource(root)
        setSource(root.companionSymbol) // module -> class, class -> module
      } catch {
        case ex: IOException =>
          signalError(ex)
        case ex: MissingRequirementError =>
          signalError(ex)
      }
      initRoot(root)
      if (!root.isPackageClass) initRoot(root.companionSymbol)
    }

    override def load(root: Symbol) { complete(root) }

    private def markAbsent(sym: Symbol): Unit = {
      val tpe: Type = if (ok) NoType else ErrorType

      if (sym != NoSymbol)
        sym setInfo tpe
    }
    private def initRoot(root: Symbol) {
      if (root.rawInfo == this)
        List(root, root.moduleClass) foreach markAbsent
      else if (root.isClass && !root.isModuleClass)
        root.rawInfo.load(root)
    }
  }

  /**
   * Load contents of a package
   */
  class PackageLoader(classpath: ClassPath[platform.BinaryRepr]) extends SymbolLoader with FlagAgnosticCompleter {
    protected def description = "package loader "+ classpath.name

    protected def doComplete(root: Symbol) {
      assert(root.isPackageClass, root)
      root.setInfo(new PackageClassInfoType(newScope, root))

      val sourcepaths = classpath.sourcepaths
      if (!root.isRoot) {
        for (classRep <- classpath.classes if platform.doLoad(classRep)) {
          initializeFromClassPath(root, classRep)
        }
      }
      if (!root.isEmptyPackageClass) {
        for (pkg <- classpath.packages) {
          enterPackage(root, pkg.name, new PackageLoader(pkg))
        }

        openPackageModule(root)
      }
    }
  }

  class ClassfileLoader(val classfile: AbstractFile) extends SymbolLoader with FlagAssigningCompleter {
    private object classfileParser extends ClassfileParser {
      val global: SymbolLoaders.this.global.type = SymbolLoaders.this.global
    }

    protected def description = "class file "+ classfile.toString

    protected def doComplete(root: Symbol) {
      val start = if (Statistics.canEnable) Statistics.startTimer(classReadNanos) else null
      classfileParser.parse(classfile, root)
      if (root.associatedFile eq null) {
        root match {
          // In fact, the ModuleSymbol forwards its setter to the module class
          case _: ClassSymbol | _: ModuleSymbol =>
            debuglog("ClassfileLoader setting %s.associatedFile = %s".format(root.name, classfile))
            root.associatedFile = classfile
          case _ =>
            debuglog("Not setting associatedFile to %s because %s is a %s".format(classfile, root.name, root.shortSymbolClass))
        }
      }
      if (Statistics.canEnable) Statistics.stopTimer(classReadNanos, start)
    }
    override def sourcefile: Option[AbstractFile] = classfileParser.srcfile
  }

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