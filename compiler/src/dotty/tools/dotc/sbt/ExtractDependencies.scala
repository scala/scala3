package dotty.tools.dotc
package sbt

import ast.{Trees, tpd}
import core._, core.Decorators._
import util.NoSource.{file => NoSourceFile}
import Contexts._, Flags._, Phases._, Trees._, Types._, Symbols._
import Names._, NameOps._, StdNames._

import scala.collection.{Set, mutable}

import dotty.tools.io
import dotty.tools.io.{AbstractFile, ZipArchive, PlainFile}

import java.io.File

import java.util.{Arrays, Comparator, EnumSet}

import xsbti.api.DependencyContext
import xsbti.api.DependencyContext._
import xsbti.UseScope


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
    if ((ctx.sbtCallback != null || forceRun) && !unit.isJava) {
      val sourceFile = unit.source.file
      val responsibleOfImports = firstClassOrModule(unit.tpdTree) match {
        case None =>
          ctx.warning("""|No class, trait or object is defined in the compilation unit.
                         |The incremental compiler cannot record the dependency information in such case.
                         |Some errors like unused import referring to a non-existent class might not be reported.
                         |""".stripMargin, unit.tpdTree.pos)
          defn.RootClass
        case Some(sym) => sym
      }
      val extractDeps = new ExtractDependenciesCollector(responsibleOfImports)
      extractDeps.traverse(unit.tpdTree)

      if (dumpInc) {
        val names = extractDeps.usedNames.map(_.toString).toArray[Object]
        val deps = extractDeps.topLevelDependencies.map(_.toString).toArray[Object]
        val inhDeps = extractDeps.topLevelInheritanceDependencies.map(_.toString).toArray[Object]
        Arrays.sort(names)
        Arrays.sort(deps)
        Arrays.sort(inhDeps)

        val pw = io.File(sourceFile.jpath).changeExtension("inc").toFile.printWriter()
        try {
          pw.println(s"// usedNames: ${names.mkString(",")}")
          pw.println(s"// topLevelDependencies: ${deps.mkString(",")}")
          pw.println(s"// topLevelInheritanceDependencies: ${inhDeps.mkString(",")}")
        } finally pw.close()
      }

      if (ctx.sbtCallback != null) {
        extractDeps.usedNames.foreach{
          case (rawClassName, usedNames) =>
            val className = rawClassName
            usedNames.defaultNames.foreach { rawUsedName =>
              val useName = rawUsedName.toString
              val useScopes =
                usedNames.scopedNames.get(rawUsedName) match {
                  case None => EnumSet.of(UseScope.Default)
                  case Some(existingScopes) =>
                    existingScopes.add(UseScope.Default)
                    existingScopes
                }

              ctx.sbtCallback.usedName(className, useName, useScopes)
            }
        }

        // FIXME: https://github.com/sbt/zinc/commit/05482d131346d645375263e1420d2cd19b2ea6ef
        extractDeps.topLevelDependencies.foreach(dep => recordDependency(dep, DependencyByMemberRef, allowLocal = true))
        extractDeps.topLevelInheritanceDependencies.foreach(dep => recordDependency(dep, DependencyByInheritance, allowLocal = true))
      }
    }
  }

  private def firstClassOrModule(tree: tpd.Tree)(implicit ctx: Context): Option[Symbol] = {
    import tpd._
    val acc = new TreeAccumulator[Option[Symbol]] {
      def apply(x: Option[Symbol], t: Tree)(implicit ctx: Context) =
        if (x.isDefined) x
        else t match {
          case moduleDef: Thicket =>
            Some(moduleDef.symbol)
          case typeDef: TypeDef =>
            Some(typeDef.symbol)
          case other =>
            foldOver(x, other)
        }
    }
    acc(None, tree)
  }

  /*
   * Handles dependency on given symbol by trying to figure out if represents a term
   * that is coming from either source code (not necessarily compiled in this compilation
   * run) or from class file and calls respective callback method.
   */
  def recordDependency(dep: ClassDependency, context: DependencyContext, allowLocal: Boolean)(implicit ctx: Context): Unit = {
    val fromClassName = classNameAsString(dep.from)
    val sourceFile = ctx.compilationUnit.source.file.file

    def binaryDependency(file: File, binaryClassName: String) =
      ctx.sbtCallback.binaryDependency(file, binaryClassName, fromClassName, sourceFile, context)

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
          val packages = dep.to.ownersIterator
            .count(x => x.is(PackageClass) && !x.isEffectiveRoot)
          // We can recover the fully qualified name of a classfile from
          // its path
          val classSegments = pf.givenPath.segments.takeRight(packages + 1)
          binaryDependency(pf.file, binaryClassName(classSegments))

        case _ =>
          ctx.warning(s"sbt-deps: Ignoring dependency $depFile of class ${depFile.getClass}}")
      }
    }

    val depFile = dep.to.associatedFile
    if (depFile != null) {
      if (depFile.extension == "class") {
        // Dependency is external -- source is undefined
        processExternalDependency(depFile)
      } else if (allowLocal || depFile.file != sourceFile) {
        // We cannot ignore dependencies coming from the same source file because
        // the dependency info needs to propagate. See source-dependencies/trait-trait-211.
        val toClassName = classNameAsString(dep.to)
        ctx.sbtCallback.classDependency(toClassName, fromClassName, context)
      }
    }
  }
}

object ExtractDependencies {
  def classNameAsString(sym: Symbol)(implicit ctx: Context): String =
    sym.fullName.stripModuleClassSuffix.toString

  def isLocal(sym: Symbol)(implicit ctx: Context): Boolean =
    sym.ownersIterator.exists(_.isTerm)
}

private case class ClassDependency(from: Symbol, to: Symbol)

private final class NameUsedInClass {
  // Default names and other scopes are separated for performance reasons
  val defaultNames: mutable.Set[Name] = new mutable.HashSet[Name]
  val scopedNames: mutable.Map[Name, EnumSet[UseScope]] = new mutable.HashMap[Name, EnumSet[UseScope]].withDefault(_ => EnumSet.noneOf(classOf[UseScope]))

  // We have to leave with commas on ends
  override def toString(): String = {
    val builder = new StringBuilder(": ")
    defaultNames.foreach { name =>
      builder.append(name.toString.trim)
      val otherScopes = scopedNames.get(name)
      scopedNames.get(name) match {
        case None =>
        case Some(otherScopes) =>
          // Pickling tests fail when this is turned in an anonymous class
          class Consumer extends java.util.function.Consumer[UseScope]() {
            override def accept(scope: UseScope): Unit =
              builder.append(scope.name()).append(", ")
          }
          builder.append(" in [")
          otherScopes.forEach(new Consumer)
          builder.append("]")
      }
      builder.append(", ")
    }
    builder.toString()
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
private class ExtractDependenciesCollector(responsibleForImports: Symbol)(implicit val ctx: Context) extends tpd.TreeTraverser { thisTreeTraverser =>
  import tpd._
  import ExtractDependencies._

  private[this] val _usedNames = new mutable.HashMap[String, NameUsedInClass]
  private[this] val _topLevelDependencies = new mutable.HashSet[ClassDependency]
  private[this] val _topLevelInheritanceDependencies = new mutable.HashSet[ClassDependency]

  /** The names used in this class, this does not include names which are only
   *  defined and not referenced.
   */
  def usedNames: collection.Map[String, NameUsedInClass] = _usedNames

  /** The set of top-level classes that the compilation unit depends on
   *  because it refers to these classes or something defined in them.
   *  This is always a superset of `topLevelInheritanceDependencies` by definition.
   */
  def topLevelDependencies: Set[ClassDependency] = _topLevelDependencies

  /** The set of top-level classes that the compilation unit extends or that
   *  contain a non-top-level class that the compilaion unit extends.
   */
  def topLevelInheritanceDependencies: Set[ClassDependency] = _topLevelInheritanceDependencies

  private def addUsedName(enclosingSym: Symbol, name: Name) = {
    val enclosingName =
      if (enclosingSym == defn.RootClass) classNameAsString(responsibleForImports)
      else classNameAsString(enclosingSym)
    val nameUsed = _usedNames.getOrElseUpdate(enclosingName, new NameUsedInClass)
    nameUsed.defaultNames += name
    // TODO: Set correct scope
    nameUsed.scopedNames(name).add(UseScope.Default)
  }

  private def addDependency(sym: Symbol)(implicit ctx: Context): Unit =
    if (!ignoreDependency(sym)) {
      val tlClass = sym.topLevelClass
      if (tlClass.ne(NoSymbol)) {
        if (currentClass == defn.RootClass) {
          _topLevelDependencies += ClassDependency(responsibleForImports, tlClass)
        } else {
          // Some synthetic type aliases like AnyRef do not belong to any class
          _topLevelDependencies += ClassDependency(currentClass, tlClass)
        }
      }
      addUsedName(nonLocalEnclosingClass(ctx.owner), sym.name.stripModuleClassSuffix)
    }

  private def nonLocalEnclosingClass(sym: Symbol)(implicit ctx: Context): Symbol =
    sym.enclosingClass match {
      case NoSymbol => NoSymbol
      case csym =>
        if (isLocal(csym))
          nonLocalEnclosingClass(csym.owner)
        else
          csym
    }

  private def ignoreDependency(sym: Symbol) =
    sym.eq(NoSymbol) ||
    sym.isEffectiveRoot ||
    sym.isAnonymousFunction ||
    sym.isAnonymousClass

  private def addInheritanceDependency(parent: Symbol)(implicit ctx: Context): Unit =
    _topLevelInheritanceDependencies += ClassDependency(currentClass, parent.topLevelClass)

  private class PatMatDependencyTraverser(ctx0: Context) extends ExtractTypesCollector(ctx0) {
    override protected def addDependency(symbol: Symbol)(implicit ctx: Context): Unit = {
      if (!ignoreDependency(symbol) && symbol.is(Sealed)) {
        val encName = nonLocalEnclosingClass(ctx.owner).fullName.stripModuleClassSuffix.toString
        val nameUsed = _usedNames.getOrElseUpdate(encName, new NameUsedInClass)

        nameUsed.defaultNames += symbol.name
        nameUsed.scopedNames(symbol.name).add(UseScope.PatMatTarget)
      }
    }
  }

  /** Traverse the tree of a source file and record the dependencies which
   *  can be retrieved using `topLevelDependencies`, `topLevelInheritanceDependencies`,
   *  and `usedNames`
   */
  override def traverse(tree: Tree)(implicit ctx: Context): Unit = {
    tree match {
      case v @ ValDef(_, tpt, _) if v.symbol.is(Case) && v.symbol.is(Synthetic) =>
        new PatMatDependencyTraverser(ctx).traverse(tpt.tpe)
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
            if (rename ne nme.WILDCARD) {
              addUsedName(nonLocalEnclosingClass(ctx.owner), rename)
            }
          case _ =>
        }
      case Inlined(call, _, _) =>
        // The inlined call is normally ignored by TreeTraverser but we need to
        // record it as a dependency
        traverse(call)
      case t: TypeTree =>
        new usedTypeTraverser(ctx).traverse(t.tpe)
      case ref: RefTree =>
        addDependency(ref.symbol)
        new usedTypeTraverser(ctx).traverse(ref.tpe)
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
  private class ExtractTypesCollector(ctx0: Context) extends TypeTraverser()(ctx0) {
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

    protected def addDependency(symbol: Symbol)(implicit ctx: Context): Unit =
      thisTreeTraverser.addDependency(symbol)
  }

  private class usedTypeTraverser(ctx0: Context) extends ExtractTypesCollector(ctx0)
}
