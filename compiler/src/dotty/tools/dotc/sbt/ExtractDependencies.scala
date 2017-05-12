package dotty.tools.dotc
package sbt

import ast.{Trees, tpd}
import core._, core.Decorators._
import Contexts._, Flags._, Phases._, Trees._, Types._, Symbols._
import Names._, NameOps._, StdNames._

import scala.collection.{Set, mutable}

import dotty.tools.io.{AbstractFile, Path, ZipArchive, PlainFile}
import java.io.File

import java.util.{Arrays, Comparator}

import xsbti.DependencyContext

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
  override def phaseName: String = "sbt-deps"

  // This phase should be run directly after `Frontend`, if it is run after
  // `PostTyper`, some dependencies will be lost because trees get simplified.
  // See the scripted test `constants` for an example where this matters.
  // TODO: Add a `Phase#runsBefore` method ?

  override def run(implicit ctx: Context): Unit = {
    val unit = ctx.compilationUnit
    val dumpInc = ctx.settings.YdumpSbtInc.value
    val forceRun = dumpInc || ctx.settings.YforceSbtPhases.value
    if ((ctx.sbtCallback != null || forceRun) && !unit.isJava) {
      val sourceFile = unit.source.file.file
      val extractDeps = new ExtractDependenciesCollector
      extractDeps.traverse(unit.tpdTree)

      if (dumpInc) {
        val names = extractDeps.usedNames.map(_.toString).toArray[Object]
        val deps = extractDeps.topLevelDependencies.map(_.toString).toArray[Object]
        val inhDeps = extractDeps.topLevelInheritanceDependencies.map(_.toString).toArray[Object]
        Arrays.sort(names)
        Arrays.sort(deps)
        Arrays.sort(inhDeps)

        val pw = Path(sourceFile).changeExtension("inc").toFile.printWriter()
        try {
          pw.println(s"// usedNames: ${names.mkString(",")}")
          pw.println(s"// topLevelDependencies: ${deps.mkString(",")}")
          pw.println(s"// topLevelInheritanceDependencies: ${inhDeps.mkString(",")}")
        } finally pw.close()
      }

      if (ctx.sbtCallback != null) {
        extractDeps.usedNames.foreach(name =>
          ctx.sbtCallback.usedName(sourceFile, name.toString))
        extractDeps.topLevelDependencies.foreach(dep =>
          recordDependency(sourceFile, dep, DependencyContext.DependencyByMemberRef))
        extractDeps.topLevelInheritanceDependencies.foreach(dep =>
          recordDependency(sourceFile, dep, DependencyContext.DependencyByInheritance))
      }
    }
  }

  /** Record that `currentSourceFile` depends on the file where `dep` was loaded from.
   *
   *  @param currentSourceFile  The source file of the current unit
   *  @param dep                The dependency
   *  @param context            Describes how `currentSourceFile` depends on `dep`
   */
  def recordDependency(currentSourceFile: File, dep: Symbol, context: DependencyContext)
      (implicit ctx: Context) = {
    val depFile = dep.associatedFile
    if (depFile != null) {
      if (depFile.path.endsWith(".class")) {
        /** Transform `List(java, lang, String.class)` into `java.lang.String` */
        def className(classSegments: List[String]) =
          classSegments.mkString(".").stripSuffix(".class")
        def binaryDependency(file: File, className: String) =
          ctx.sbtCallback.binaryDependency(file, className, currentSourceFile, context)

        depFile match {
          case ze: ZipArchive#Entry =>
            for (zip <- ze.underlyingSource; zipFile <- Option(zip.file)) {
              val classSegments = Path(ze.path).segments
              binaryDependency(zipFile, className(classSegments))
            }
          case pf: PlainFile =>
            val packages = dep.ownersIterator
              .filter(x => x.is(PackageClass) && !x.isEffectiveRoot).length
            // We can recover the fully qualified name of a classfile from
            // its path
            val classSegments = pf.givenPath.segments.takeRight(packages + 1)
            binaryDependency(pf.file, className(classSegments))
          case _ =>
            ctx.warning(s"sbt-deps: Ignoring dependency $depFile of class ${depFile.getClass}")
        }
      } else if (depFile.file != currentSourceFile) {
        ctx.sbtCallback.sourceDependency(depFile.file, currentSourceFile, context)
      }
    }
  }
}

/** Extract the dependency information of a compilation unit.
 *
 *  To understand why we track the used names see the section "Name hashing
 *  algorithm" in http://www.scala-sbt.org/0.13/docs/Understanding-Recompilation.html
 *  To understand why we need to track dependencies introduced by inheritance
 *  specially, see the subsection "Dependencies introduced by member reference and
 *  inheritance" in the "Name hashing algorithm" section.
 */
private class ExtractDependenciesCollector(implicit val ctx: Context) extends tpd.TreeTraverser {
  import tpd._

  private[this] val _usedNames = new mutable.HashSet[Name]
  private[this] val _topLevelDependencies = new mutable.HashSet[Symbol]
  private[this] val _topLevelInheritanceDependencies = new mutable.HashSet[Symbol]

  /** The names used in this class, this does not include names which are only
   *  defined and not referenced.
   */
  def usedNames: Set[Name] = _usedNames

  /** The set of top-level classes that the compilation unit depends on
   *  because it refers to these classes or something defined in them.
   *  This is always a superset of `topLevelInheritanceDependencies` by definition.
   */
  def topLevelDependencies: Set[Symbol] = _topLevelDependencies

  /** The set of top-level classes that the compilation unit extends or that
   *  contain a non-top-level class that the compilaion unit extends.
   */
  def topLevelInheritanceDependencies: Set[Symbol] = _topLevelInheritanceDependencies

  private def addUsedName(name: Name) =
    _usedNames += name

  private def addDependency(sym: Symbol): Unit =
    if (!ignoreDependency(sym)) {
      val tlClass = sym.topLevelClass
      if (tlClass.ne(NoSymbol)) // Some synthetic type aliases like AnyRef do not belong to any class
        _topLevelDependencies += sym.topLevelClass
      addUsedName(sym.name)
    }

  private def ignoreDependency(sym: Symbol) =
    sym.eq(NoSymbol) ||
    sym.isEffectiveRoot ||
    sym.isAnonymousFunction ||
    sym.isAnonymousClass

  private def addInheritanceDependency(sym: Symbol): Unit =
    _topLevelInheritanceDependencies += sym.topLevelClass

  /** Traverse the tree of a source file and record the dependencies which
   *  can be retrieved using `topLevelDependencies`, `topLevelInheritanceDependencies`,
   *  and `usedNames`
   */
  override def traverse(tree: Tree)(implicit ctx: Context): Unit = {
    tree match {
      case Import(expr, selectors) =>
        def lookupImported(name: Name) = expr.tpe.member(name).symbol
        def addImported(name: Name) = {
          // importing a name means importing both a term and a type (if they exist)
          addDependency(lookupImported(name.toTermName))
          addDependency(lookupImported(name.toTypeName))
        }
        selectors foreach {
          case Ident(name) =>
            addImported(name)
          case Thicket(Ident(name) :: Ident(rename) :: Nil) =>
            addImported(name)
            if (rename ne nme.WILDCARD)
              addUsedName(rename)
          case _ =>
        }
      case Inlined(call, _, _) =>
        // The inlined call is normally ignored by TreeTraverser but we need to
        // record it as a dependency
        traverse(call)
      case t: TypeTree =>
        usedTypeTraverser.traverse(t.tpe)
      case ref: RefTree =>
        addDependency(ref.symbol)
        usedTypeTraverser.traverse(ref.tpe)
      case t @ Template(_, parents, _, _) =>
        t.parents.foreach(p => addInheritanceDependency(p.tpe.classSymbol))
      case _ =>
    }
    traverseChildren(tree)
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
  private object usedTypeTraverser extends TypeTraverser {
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
}
