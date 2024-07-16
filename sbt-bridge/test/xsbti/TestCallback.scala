// Taken from https://github.com/sbt/zinc/blob/aa1c04f445092e87f76aaceee4da61ea0724419e/internal/zinc-testing/src/main/scala/xsbti/TestCallback.scala
package xsbti

import java.io.File
import java.nio.file.Path
import scala.collection.mutable.ArrayBuffer
import xsbti.VirtualFileRef
import xsbti.api.ClassLike
import xsbti.api.DependencyContext
import DependencyContext._
import java.{util => ju}
import ju.Optional

class TestCallbackNoInc extends TestCallback {
  override def enabled(): Boolean = false
}

class TestCallback extends AnalysisCallback2 {
  case class TestUsedName(name: String, scopes: ju.EnumSet[UseScope])

  val classDependencies = new ArrayBuffer[(String, String, DependencyContext)]
  val binaryDependencies =
    new ArrayBuffer[(Path, String, String, VirtualFileRef, DependencyContext)]
  val productClassesToSources =
    scala.collection.mutable.Map.empty[Path, VirtualFileRef]
  val usedNamesAndScopes = scala.collection.mutable.Map
    .empty[String, Set[TestUsedName]]
    .withDefaultValue(Set.empty)
  val classNames = scala.collection.mutable.Map
    .empty[VirtualFileRef, Set[(String, String)]]
    .withDefaultValue(Set.empty)
  val apis: scala.collection.mutable.Map[VirtualFileRef, Seq[ClassLike]] =
    scala.collection.mutable.Map.empty

  def usedNames = usedNamesAndScopes.view.mapValues(_.map(_.name)).toMap

  override def startSource(source: File): Unit = ???
  override def startSource(source: VirtualFile): Unit = {
    assert(
      !apis.contains(source),
      s"startSource can be called only once per source file: $source"
    )
    apis(source) = Seq.empty
  }

  override def binaryDependency(
      binary: File,
      name: String,
      fromClassName: String,
      source: File,
      context: DependencyContext
  ): Unit = ???
  override def binaryDependency(
      binary: Path,
      name: String,
      fromClassName: String,
      source: VirtualFileRef,
      context: DependencyContext
  ): Unit = {
    binaryDependencies += ((binary, name, fromClassName, source, context))
  }

  override def generatedNonLocalClass(
      source: File,
      module: File,
      binaryClassName: String,
      srcClassName: String
  ): Unit = ???

  override def generatedNonLocalClass(
      sourceFile: VirtualFileRef,
      classFile: Path,
      binaryClassName: String,
      srcClassName: String
  ): Unit = {
    productClassesToSources += ((classFile, sourceFile))
    classNames(sourceFile) += ((srcClassName, binaryClassName))
    ()
  }

  override def generatedLocalClass(source: File, module: File): Unit = ???
  override def generatedLocalClass(
      sourceFile: VirtualFileRef,
      classFile: Path
  ): Unit = {
    productClassesToSources += ((classFile, sourceFile))
    ()
  }

  override def classDependency(
      onClassName: String,
      sourceClassName: String,
      context: DependencyContext
  ): Unit = {
    if (onClassName != sourceClassName)
      classDependencies += ((onClassName, sourceClassName, context))
  }

  override def usedName(
      className: String,
      name: String,
      scopes: ju.EnumSet[UseScope]
  ): Unit = {
    usedNamesAndScopes(className) += TestUsedName(name, scopes)
  }

  override def api(source: File, classApi: ClassLike): Unit = ???
  override def api(source: VirtualFileRef, classApi: ClassLike): Unit = {
    apis(source) = classApi +: apis(source)
  }

  override def problem(
      category: String,
      pos: xsbti.Position,
      message: String,
      severity: xsbti.Severity,
      reported: Boolean
  ): Unit = ()
  override def problem2(
      category: String,
      pos: Position,
      msg: String,
      severity: Severity,
      reported: Boolean,
      rendered: Optional[String],
      diagnosticCode: Optional[xsbti.DiagnosticCode],
      diagnosticRelatedInformation: ju.List[xsbti.DiagnosticRelatedInformation],
      actions: ju.List[xsbti.Action]
  ): Unit = ()
  override def dependencyPhaseCompleted(): Unit = ()
  override def apiPhaseCompleted(): Unit = ()
  override def enabled(): Boolean = true

  override def mainClass(source: File, className: String): Unit = ()
  override def mainClass(source: VirtualFileRef, className: String): Unit = ???

  override def classesInOutputJar(): java.util.Set[String] = ???
  override def getPickleJarPair(): java.util.Optional[xsbti.T2[Path, Path]] =
    ???
  override def isPickleJava(): Boolean = ???
}

object TestCallback {
  case class ExtractedClassDependencies(
      memberRef: Map[String, Set[String]],
      inheritance: Map[String, Set[String]],
      localInheritance: Map[String, Set[String]]
  )
  object ExtractedClassDependencies {
    def fromPairs(
        memberRefPairs: collection.Seq[(String, String)],
        inheritancePairs: collection.Seq[(String, String)],
        localInheritancePairs: collection.Seq[(String, String)]
    ): ExtractedClassDependencies = {
      ExtractedClassDependencies(
        pairsToMultiMap(memberRefPairs),
        pairsToMultiMap(inheritancePairs),
        pairsToMultiMap(localInheritancePairs)
      )
    }

    private def pairsToMultiMap[A, B](
        pairs: collection.Seq[(A, B)]
    ): Map[A, Set[B]] = {
      pairs
        .groupBy(_._1)
        .view
        .mapValues(values => values.map(_._2).toSet)
        .toMap
        .withDefaultValue(Set.empty)
    }
  }
}
