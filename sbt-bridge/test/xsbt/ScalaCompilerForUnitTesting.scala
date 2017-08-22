/** Adapted from https://github.com/sbt/sbt/blob/0.13/compile/interface/src/test/scala/xsbt/ScalaCompilerForUnitTesting.scala */
package xsbt

import xsbti.compile.SingleOutput
import java.io.File
import xsbti._
import xsbti.api.SourceAPI
import sbt.IO._
import xsbti.api.ClassLike
import xsbti.api.Definition
import xsbti.api.Def
import xsbt.api.SameAPI
import sbt.ConsoleLogger
import xsbti.DependencyContext._

import ScalaCompilerForUnitTesting.ExtractedSourceDependencies

/**
 * Provides common functionality needed for unit tests that require compiling
 * source code using Scala compiler.
 */
class ScalaCompilerForUnitTesting(nameHashing: Boolean, includeSynthToNameHashing: Boolean = false) {
  import scala.language.reflectiveCalls

  /**
   * Compiles given source code using Scala compiler and returns API representation
   * extracted by ExtractAPI class.
   */
  def extractApiFromSrc(src: String): SourceAPI = {
    val (Seq(tempSrcFile), analysisCallback) = compileSrcs(src)
    analysisCallback.apis(tempSrcFile)
  }

  /**
   * Compiles given source code using Scala compiler and returns API representation
   * extracted by ExtractAPI class.
   */
  def extractApisFromSrcs(reuseCompilerInstance: Boolean)(srcs: List[String]*): Seq[SourceAPI] = {
    val (tempSrcFiles, analysisCallback) = compileSrcs(srcs.toList, reuseCompilerInstance)
    tempSrcFiles.map(analysisCallback.apis)
  }

  def extractUsedNamesFromSrc(src: String): Set[String] = {
    val (Seq(tempSrcFile), analysisCallback) = compileSrcs(src)
    analysisCallback.usedNames(tempSrcFile)
  }

  /**
   * Extract used names from src provided as the second argument.
   *
   * The purpose of the first argument is to define names that the second
   * source is going to refer to. Both files are compiled in the same compiler
   * Run but only names used in the second src file are returned.
   */
  def extractUsedNamesFromSrc(definitionSrc: String, actualSrc: String): Set[String] = {
    // we drop temp src file corresponding to the definition src file
    val (Seq(_, tempSrcFile), analysisCallback) = compileSrcs(definitionSrc, actualSrc)
    analysisCallback.usedNames(tempSrcFile)
  }

  /**
   * Compiles given source code snippets (passed as Strings) using Scala compiler and returns extracted
   * dependencies between snippets. Source code snippets are identified by symbols. Each symbol should
   * be associated with one snippet only.
   *
   * Snippets can be grouped to be compiled together in the same compiler run. This is
   * useful to compile macros, which cannot be used in the same compilation run that
   * defines them.
   *
   * Symbols are used to express extracted dependencies between source code snippets. This way we have
   * file system-independent way of testing dependencies between source code "files".
   */
  def extractDependenciesFromSrcs(srcs: List[Map[Symbol, String]]): ExtractedSourceDependencies = {
    val rawGroupedSrcs = srcs.map(_.values.toList)
    val symbols = srcs.flatMap(_.keys)
    val (tempSrcFiles, testCallback) = compileSrcs(rawGroupedSrcs, reuseCompilerInstance = true)
    val fileToSymbol = (tempSrcFiles zip symbols).toMap

    val memberRefFileDeps = testCallback.sourceDependencies collect {
      // false indicates that those dependencies are not introduced by inheritance
      case (target, src, DependencyByMemberRef) => (src, target)
    }
    val inheritanceFileDeps = testCallback.sourceDependencies collect {
      // true indicates that those dependencies are introduced by inheritance
      case (target, src, DependencyByInheritance) => (src, target)
    }
    def toSymbols(src: File, target: File): (Symbol, Symbol) = (fileToSymbol(src), fileToSymbol(target))
    val memberRefDeps = memberRefFileDeps map { case (src, target) => toSymbols(src, target) }
    val inheritanceDeps = inheritanceFileDeps map { case (src, target) => toSymbols(src, target) }
    def pairsToMultiMap[A, B](pairs: Seq[(A, B)]): Map[A, Set[B]] = {
      import scala.collection.mutable.{ HashMap, MultiMap }
      val emptyMultiMap = new HashMap[A, scala.collection.mutable.Set[B]] with MultiMap[A, B]
      val multiMap = pairs.foldLeft(emptyMultiMap) {
        case (acc, (key, value)) =>
          acc.addBinding(key, value)
      }
      // convert all collections to immutable variants
      multiMap.toMap.mapValues(_.toSet).withDefaultValue(Set.empty)
    }

    ExtractedSourceDependencies(pairsToMultiMap(memberRefDeps), pairsToMultiMap(inheritanceDeps))
  }

  def extractDependenciesFromSrcs(srcs: (Symbol, String)*): ExtractedSourceDependencies = {
    val symbols = srcs.map(_._1)
    assert(symbols.distinct.size == symbols.size,
      s"Duplicate symbols for srcs detected: $symbols")
    extractDependenciesFromSrcs(List(srcs.toMap))
  }

  /**
   * Compiles given source code snippets written to temporary files. Each snippet is
   * written to a separate temporary file.
   *
   * Snippets can be grouped to be compiled together in the same compiler run. This is
   * useful to compile macros, which cannot be used in the same compilation run that
   * defines them.
   *
   * The `reuseCompilerInstance` parameter controls whether the same Scala compiler instance
   * is reused between compiling source groups. Separate compiler instances can be used to
   * test stability of API representation (with respect to pickling) or to test handling of
   * binary dependencies.
   *
   * The sequence of temporary files corresponding to passed snippets and analysis
   * callback is returned as a result.
   */
  private def compileSrcs(groupedSrcs: List[List[String]],
    reuseCompilerInstance: Boolean): (Seq[File], TestCallback) = {
    // withTemporaryDirectory { temp =>
    {
      val temp = createTemporaryDirectory
      val analysisCallback = new TestCallback(nameHashing, includeSynthToNameHashing)
      val classesDir = new File(temp, "classes")
      classesDir.mkdir()

      lazy val commonCompilerInstanceAndCtx = prepareCompiler(classesDir, analysisCallback, classesDir.toString)

      val files = for ((compilationUnit, unitId) <- groupedSrcs.zipWithIndex) yield {
        // use a separate instance of the compiler for each group of sources to
        // have an ability to test for bugs in instability between source and pickled
        // representation of types
        val (compiler, ctx) = if (reuseCompilerInstance) commonCompilerInstanceAndCtx else
          prepareCompiler(classesDir, analysisCallback, classesDir.toString)
        val run = compiler.newRun(ctx)
        val srcFiles = compilationUnit.toSeq.zipWithIndex map {
          case (src, i) =>
            val fileName = s"Test-$unitId-$i.scala"
            prepareSrcFile(temp, fileName, src)
        }
        val srcFilePaths = srcFiles.map(srcFile => srcFile.getAbsolutePath).toList

        run.compile(srcFilePaths)

        // srcFilePaths.foreach(f => new File(f).delete)
        srcFiles
      }
      (files.flatten.toSeq, analysisCallback)
    }
  }

  private def compileSrcs(srcs: String*): (Seq[File], TestCallback) = {
    compileSrcs(List(srcs.toList), reuseCompilerInstance = true)
  }

  private def prepareSrcFile(baseDir: File, fileName: String, src: String): File = {
    val srcFile = new File(baseDir, fileName)
    sbt.IO.write(srcFile, src)
    srcFile
  }

  private def prepareCompiler(outputDir: File, analysisCallback: AnalysisCallback, classpath: String = ".") = {
    val args = Array.empty[String]

    import dotty.tools.dotc.{Compiler, Driver}
    import dotty.tools.dotc.core.Contexts._

    val driver = new TestDriver
    val ctx = (new ContextBase).initialCtx.fresh.setSbtCallback(analysisCallback)
    driver.getCompiler(Array("-classpath", classpath, "-usejavacp", "-d", outputDir.getAbsolutePath), ctx)
  }

  private object ConsoleReporter extends Reporter {
    def reset(): Unit = ()
    def hasErrors: Boolean = false
    def hasWarnings: Boolean = false
    def printWarnings(): Unit = ()
    def problems: Array[Problem] = Array.empty
    def log(pos: Position, msg: String, sev: Severity): Unit = println(msg)
    def comment(pos: Position, msg: String): Unit = ()
    def printSummary(): Unit = ()
  }

}

object ScalaCompilerForUnitTesting {
  case class ExtractedSourceDependencies(memberRef: Map[Symbol, Set[Symbol]], inheritance: Map[Symbol, Set[Symbol]])
}
