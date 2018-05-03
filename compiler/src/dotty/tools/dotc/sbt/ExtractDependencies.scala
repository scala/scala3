package dotty.tools.dotc
package sbt

import java.io.File
import java.util.{Arrays, EnumSet}

import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.NameOps._
import dotty.tools.dotc.core.Names._
import dotty.tools.dotc.core.Phases._
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.transform.SymUtils._
import dotty.tools.io
import dotty.tools.io.{AbstractFile, PlainFile, ZipArchive}
import xsbti.UseScope
import xsbti.api.DependencyContext
import xsbti.api.DependencyContext._

import scala.collection.{Set, mutable}


/** This phase sends information on classes' dependencies to sbt via callbacks.
 *
 *  This is used by sbt for incremental recompilation. Briefly, when a file
 *  changes sbt will recompile it, if its API has changed (determined by what
 *  `ExtractAPI` sent) then sbt will determine which reverse-dependencies
 *  (determined by what `ExtractDependencies` sent) of the API have to be
 *  recompiled depending on what changed.
 *
 *  See the documentation of `ExtractDependenciesCollector`, `ExtractAPI`,
 *  `ExtractAPICollector` and
 *  http://www.scala-sbt.org/0.13/docs/Understanding-Recompilation.html for more
 *  information on how sbt incremental compilation works.
 *
 *  The following flags affect this phase:
 *   -Yforce-sbt-phases
 *   -Ydump-sbt-inc
 *
 *  @see ExtractAPI
 */
class ExtractDependencies extends Phase {
  import ExtractDependencies._

  override def phaseName: String = "sbt-deps"

  // This phase should be run directly after `Frontend`, if it is run after
  // `PostTyper`, some dependencies will be lost because trees get simplified.
  // See the scripted test `constants` for an example where this matters.
  // TODO: Add a `Phase#runsBefore` method ?

  override def run(implicit ctx: Context): Unit = {
    val unit = ctx.compilationUnit
    val dumpInc = ctx.settings.YdumpSbtInc.value
    val forceRun = dumpInc || ctx.settings.YforceSbtPhases.value
    val shouldRun = !unit.isJava && (ctx.sbtCallback != null || forceRun)

    if (shouldRun) {
      val collector = new ExtractDependenciesCollector
      collector.traverse(unit.tpdTree)

      if (dumpInc) {
        val deps = collector.dependencies.map(_.toString).toArray[Object]
        val names = collector.usedNames.map(_.toString).toArray[Object]
        Arrays.sort(deps)
        Arrays.sort(names)

        val pw = io.File(unit.source.file.jpath).changeExtension("inc").toFile.printWriter()
        // val pw = Console.out
        try {
          pw.println("Used Names:")
          pw.println("===========")
          names.foreach(pw.println)
          pw.println()
          pw.println("Dependencies:")
          pw.println("=============")
          deps.foreach(pw.println)
        } finally pw.close()
      }

      if (ctx.sbtCallback != null) {
        collector.usedNames.foreach(name => ()
          /* ctx.sbtCallback.usedName(sourceFile.file, name.toString) */ )
        collector.dependencies.foreach(dep => recordDependency(dep))
      }
    }
  }

  /*
   * Handles dependency on given symbol by trying to figure out if represents a term
   * that is coming from either source code (not necessarily compiled in this compilation
   * run) or from class file and calls respective callback method.
   */
  def recordDependency(dep: ClassDependency)(implicit ctx: Context): Unit = {
    val sourceFile = ctx.compilationUnit.source.file.file

    def binaryDependency(file: File, binaryClassName: String) = ()
      // ctx.sbtCallback.binaryDependency(file, binaryClassName, sourceFile, dep.context)

    def processExternalDependency(depFile: AbstractFile) = {
      def binaryClassName(classSegments: List[String]) =
        classSegments.mkString(".").stripSuffix(".class")

      depFile match {
        case ze: ZipArchive#Entry => // The dependency comes from a JAR
          for (zip <- ze.underlyingSource; zipFile <- Option(zip.file)) {
            val classSegments = io.File(ze.path).segments
            binaryDependency(zipFile, binaryClassName(classSegments))
          }

        case pf: PlainFile => // The dependency comes from a class file
          val packages = dep.dep.ownersIterator
            .count(x => x.is(PackageClass) && !x.isEffectiveRoot)
          // We can recover the fully qualified name of a classfile from
          // its path
          val classSegments = pf.givenPath.segments.takeRight(packages + 1)
          binaryDependency(pf.file, binaryClassName(classSegments))

        case _ =>
          ctx.warning(s"sbt-deps: Ignoring dependency $depFile of class ${depFile.getClass}}")
      }
    }

    val depFile = dep.dep.associatedFile
    if (depFile != null) {
      // Cannot ignore inheritance relationship coming from the same source (see sbt/zinc#417)
      def allowLocal = dep.context == DependencyByInheritance || dep.context == LocalDependencyByInheritance
      if (depFile.extension == "class") {
        // Dependency is external -- source is undefined
        processExternalDependency(depFile)
      } else if (allowLocal || depFile.file != sourceFile) {
        // We cannot ignore dependencies coming from the same source file because
        // the dependency info needs to propagate. See source-dependencies/trait-trait-211.
        val toClassName = classNameAsString(dep.dep)
        // ctx.sbtCallback.classDependency(toClassName, dep.context)
      }
    }
  }
}

object ExtractDependencies {
  def classNameAsString(sym: Symbol)(implicit ctx: Context): String =
    sym.fullName.stripModuleClassSuffix.toString
}

private case class ClassDependency(dep: Symbol, context: DependencyContext)

/** Extract the dependency information of a compilation unit.
 *
 *  To understand why we track the used names see the section "Name hashing
 *  algorithm" in http://www.scala-sbt.org/0.13/docs/Understanding-Recompilation.html
 *  To understand why we need to track dependencies introduced by inheritance
 *  specially, see the subsection "Dependencies introduced by member reference and
 *  inheritance" in the "Name hashing algorithm" section.
 */
private class ExtractDependenciesCollector extends tpd.TreeTraverser { thisTreeTraverser =>
  import tpd._

  private[this] val _usedNames = new mutable.HashSet[Name]
  private[this] val _dependencies = new mutable.HashSet[ClassDependency]

  /** The names used in this class, this does not include names which are only
   *  defined and not referenced.
   */
  def usedNames: collection.Set[Name] = _usedNames

  /** The set of class dependencies from this compilation unit.
   */
  def dependencies: collection.Set[ClassDependency] = _dependencies

  private def addUsedName(name: Name): Unit = {
    _usedNames.add(name)
  }

  /** Mangle a JVM symbol name in a format better suited for internal uses by sbt. */
  private def mangledName(sym: Symbol)(implicit ctx: Context): Name = {
    def constructorName = sym.owner.fullName ++ ";init;"

    if (sym.isConstructor) constructorName
    else sym.name.stripModuleClassSuffix
  }

  private def addMemberRefDependency(sym: Symbol)(implicit ctx: Context): Unit =
    if (!ignoreDependency(sym)) {
      val tlClass = sym.topLevelClass

      if (tlClass.exists) // Some synthetic type aliases like AnyRef do not belong to any class
        _dependencies += ClassDependency(tlClass, DependencyByMemberRef)

      addUsedName(mangledName(sym))
    }

  private def addInheritanceDependencies(tree: Template)(implicit ctx: Context): Unit =
    if (tree.parents.nonEmpty) {
      tree.parents.foreach { parent =>
        val dep = parent.tpe.classSymbol.topLevelClass
        _dependencies += ClassDependency(dep, DependencyByInheritance)
      }
    }

  private def ignoreDependency(sym: Symbol)(implicit ctx: Context) =
    !sym.exists ||
    sym.unforcedIsAbsent || // ignore dependencies that have a symbol but do not exist.
                            // e.g. java.lang.Object companion object
    sym.isEffectiveRoot ||
    sym.isAnonymousFunction ||
    sym.isAnonymousClass

  /** Traverse the tree of a source file and record the dependencies and used names which
   *  can be retrieved using `dependencies` and`usedNames`.
   */
  override def traverse(tree: Tree)(implicit ctx: Context): Unit = {
    tree match {
      case Import(expr, selectors) =>
        def lookupImported(name: Name) = expr.tpe.member(name).symbol
        def addImported(name: Name) = {
          // importing a name means importing both a term and a type (if they exist)
          addMemberRefDependency(lookupImported(name.toTermName))
          addMemberRefDependency(lookupImported(name.toTypeName))
        }
        selectors.foreach {
          case Ident(name) =>
            addImported(name)
          case Thicket(Ident(name) :: Ident(rename) :: Nil) =>
            addImported(name)
            if (rename ne nme.WILDCARD) {
              addUsedName(rename)
            }
          case _ =>
        }
      case t: TypeTree =>
        addTypeDependency(t.tpe)
      case ref: RefTree =>
        addMemberRefDependency(ref.symbol)
        addTypeDependency(ref.tpe)
      case t: Template =>
        addInheritanceDependencies(t)
      case _ =>
    }

    tree match {
      case Inlined(call, _, _) =>
        // The inlined call is normally ignored by TreeTraverser but we need to
        // record it as a dependency
        traverse(call)
      case vd: ValDef if vd.symbol.is(ModuleVal) =>
        // Don't visit module val
      case t: Template if t.symbol.owner.is(ModuleClass) =>
        // Don't visit self type of module class
        traverse(t.constr)
        t.parents.foreach(traverse)
        t.body.foreach(traverse)
      case _ =>
        traverseChildren(tree)
    }
  }

  /** Traverse a used type and record all the dependencies we need to keep track
   *  of for incremental recompilation.
   *
   *  As a motivating example, given a type `T` defined as:
   *
   *    type T >: L <: H
   *    type L <: A1
   *    type H <: B1
   *    class A1 extends A0
   *    class B1 extends B0
   *
   *  We need to record a dependency on `T`, `L`, `H`, `A1`, `B1`. This is
   *  necessary because the API representation that `ExtractAPI` produces for
   *  `T` just refers to the strings "L" and "H", it does not contain their API
   *  representation. Therefore, the name hash of `T` does not change if for
   *  example the definition of `L` changes.
   *
   *  We do not need to keep track of superclasses like `A0` and `B0` because
   *  the API representation of a class (and therefore its name hash) already
   *  contains all necessary information on superclasses.
   *
   *  A natural question to ask is: Since traversing all referenced types to
   *  find all these names is costly, why not change the API representation
   *  produced by `ExtractAPI` to contain that information? This way the name
   *  hash of `T` would change if any of the types it depends on change, and we
   *  would only need to record a dependency on `T`. Unfortunately there is no
   *  simple answer to the question "what does T depend on?" because it depends
   *  on the prefix and `ExtractAPI` does not compute types as seen from every
   *  possible prefix, the documentation of `ExtractAPI` explains why.
   *
   *  The tests in sbt `types-in-used-names-a`, `types-in-used-names-b`,
   *  `as-seen-from-a` and `as-seen-from-b` rely on this.
   */
  private abstract class TypeDependencyTraverser(implicit ctx: Context) extends TypeTraverser()(ctx) {
    protected def addDependency(symbol: Symbol): Unit

    val seen = new mutable.HashSet[Type]
    def traverse(tp: Type): Unit = if (!seen.contains(tp)) {
      seen += tp
      tp match {
        case tp: NamedType =>
          val sym = tp.symbol
          if (!sym.is(Package)) {
            addDependency(sym)
            if (!sym.isClass)
              traverse(tp.info)
            traverse(tp.prefix)
          }
        case tp: ThisType =>
          traverse(tp.underlying)
        case tp: ConstantType =>
          traverse(tp.underlying)
        case tp: ParamRef =>
          traverse(tp.underlying)
        case _ =>
          traverseChildren(tp)
      }
    }
  }

  def addTypeDependency(tpe: Type)(implicit ctx: Context) = {
    val traverser = new TypeDependencyTraverser {
      def addDependency(symbol: Symbol) = addMemberRefDependency(symbol)
    }
    traverser.traverse(tpe)
  }
}
