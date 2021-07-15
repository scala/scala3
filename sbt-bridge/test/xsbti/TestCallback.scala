/** Copied from https://github.com/sbt/sbt/blob/0.13/interface/src/test/scala/xsbti/TestCallback.scala */
package xsbti

import java.io.File
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import xsbti.api.ClassLike
import xsbti.api.DependencyContext

import java.nio.file.Path
import java.util
import java.util.{EnumSet, Optional}

class TestCallback extends AnalysisCallback {

  type MyFile = File

  case class TestUsedName(name: String, scopes: EnumSet[UseScope])
  val classDependencies = new ArrayBuffer[(String, String, DependencyContext)]
  val binaryDependencies = new ArrayBuffer[(MyFile, String, String, MyFile, DependencyContext)]
  val products = new ArrayBuffer[(MyFile, MyFile)]
  val usedNamesAndScopes = mutable.Map.empty[String, Set[TestUsedName]].withDefaultValue(Set.empty)
  val classNames = mutable.Map.empty[MyFile, Set[(String, String)]].withDefaultValue(Set.empty)
  val apis: mutable.Map[MyFile, Seq[ClassLike]] = mutable.Map.empty.withDefaultValue(Seq.empty)

  def usedNames = usedNamesAndScopes.view.mapValues(_.map(_.name)).toMap

  override def startSource(source: File): Unit = {
    assert(!apis.contains(source), s"startSource can be called only once per source file: $source")
    apis(source) = Seq.empty
  }

  override def binaryDependency(binary: File, name: String, fromClassName: String, source: File, context: DependencyContext): Unit = {
    binaryDependencies += ((binary, name, fromClassName, source, context))
  }

  def generatedNonLocalClass(source: File,
                             module: File,
                             binaryClassName: String,
                             srcClassName: String): Unit = {
    products += ((source, module))
    classNames(source) += ((srcClassName, binaryClassName))
    ()
  }

  def generatedLocalClass(source: File, module: File): Unit = {
    products += ((source, module))
    ()
  }


  override def classDependency(onClassName: String, sourceClassName: String, context: DependencyContext): Unit = {
    if (onClassName != sourceClassName) classDependencies += ((onClassName, sourceClassName, context))
  }

  override def usedName(className: String, name: String, scopes: EnumSet[UseScope]): Unit = {
    usedNamesAndScopes(className) += TestUsedName(name, scopes)
  }
  override def api(source: File, classApi: ClassLike): Unit = {
    apis(source) = classApi +: apis(source)
  }
  override def problem(category: String, pos: xsbti.Position, message: String, severity: xsbti.Severity, reported: Boolean): Unit = ()
  override def dependencyPhaseCompleted(): Unit = ()
  override def apiPhaseCompleted(): Unit = ()
  override def enabled(): Boolean = true
  def mainClass(source: File, className: String): Unit = ()

  // not used (we have to use this version of zinc-apiinfo, because scala 2.13 is only supported since version 1.4)
  override def api(sourceFile: VirtualFileRef, classApi: ClassLike): Unit = ???
  override def startSource(source: VirtualFile): Unit = ???
  override def binaryDependency(onBinaryEntry: Path, onBinaryClassName: String, fromClassName: String, fromSourceFile: VirtualFileRef, context: DependencyContext): Unit = ???
  override def generatedNonLocalClass(source: VirtualFileRef, classFile: Path, binaryClassName: String, srcClassName: String): Unit = ???
  override def generatedLocalClass(source: VirtualFileRef, classFile: Path): Unit = ???
  override def mainClass(sourceFile: VirtualFileRef, className: String): Unit = ???
  override def classesInOutputJar(): util.Set[String] = ???
  override def isPickleJava: Boolean = ???
  override def getPickleJarPair: Optional[T2[Path, Path]] = ???
}

object TestCallback {
  case class ExtractedClassDependencies(memberRef: Map[String, Set[String]],
                                        inheritance: Map[String, Set[String]],
                                        localInheritance: Map[String, Set[String]])
  object ExtractedClassDependencies {
    def fromPairs(
                   memberRefPairs: collection.Seq[(String, String)],
                   inheritancePairs: collection.Seq[(String, String)],
                   localInheritancePairs: collection.Seq[(String, String)]
                 ): ExtractedClassDependencies = {
      ExtractedClassDependencies(pairsToMultiMap(memberRefPairs),
        pairsToMultiMap(inheritancePairs),
        pairsToMultiMap(localInheritancePairs))
    }

    private def pairsToMultiMap[A, B](pairs: collection.Seq[(A, B)]): Map[A, Set[B]] = {
      import mutable.{ HashMap, MultiMap }
      val emptyMultiMap = new HashMap[A, mutable.Set[B]] with MultiMap[A, B]
      val multiMap = pairs.foldLeft(emptyMultiMap) {
        case (acc, (key, value)) =>
          acc.addBinding(key, value)
      }
      // convert all collections to immutable variants
      multiMap.toMap.view.mapValues(_.toSet).toMap.withDefaultValue(Set.empty)
    }
  }
}

