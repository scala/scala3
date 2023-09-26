/** Adapted from https://github.com/sbt/sbt/blob/0.13/compile/interface/src/test/scala/xsbt/ScalaCompilerForUnitTesting.scala */
package xsbt

import xsbti.compile.{CompileProgress, SingleOutput}
import java.io.File
import xsbti._
import sbt.io.IO
import xsbti.api.{ ClassLike, Def, DependencyContext }
import DependencyContext._
import xsbt.api.SameAPI
import sbt.internal.util.ConsoleLogger
import dotty.tools.io.PlainFile.toPlainFile
import dotty.tools.xsbt.CompilerBridge

import TestCallback.ExtractedClassDependencies

/**
 * Provides common functionality needed for unit tests that require compiling
 * source code using Scala compiler.
 */
class ScalaCompilerForUnitTesting {

  /**
   * Compiles given source code using Scala compiler and returns API representation
   * extracted by ExtractAPI class.
   */
  def extractApiFromSrc(src: String): Seq[ClassLike] = {
    val (Seq(tempSrcFile), analysisCallback) = compileSrcs(src)
    analysisCallback.apis(tempSrcFile)
  }

  /**
   * Compiles given source code using Scala compiler and returns API representation
   * extracted by ExtractAPI class.
   */
  def extractApisFromSrcs(srcs: List[String]*): Seq[Seq[ClassLike]] = {
    val (tempSrcFiles, analysisCallback) = compileSrcs(srcs.toList)
    tempSrcFiles.map(analysisCallback.apis)
  }

  /**
   * Extract used names from src provided as the second argument.
   * If `assertDefaultScope` is set to true it will fail if there is any name used in scope other then Default
   *
   * The purpose of the first argument is to define names that the second
   * source is going to refer to. Both files are compiled in the same compiler
   * Run but only names used in the second src file are returned.
   */
  def extractUsedNamesFromSrc(
      definitionSrc: String,
      actualSrc: String,
      assertDefaultScope: Boolean = true
  ): Map[String, Set[String]] = {
    // we drop temp src file corresponding to the definition src file
    val (Seq(_, tempSrcFile), analysisCallback) = compileSrcs(definitionSrc, actualSrc)

    if (assertDefaultScope) for {
      (className, used) <- analysisCallback.usedNamesAndScopes
      analysisCallback.TestUsedName(name, scopes) <- used
    } assert(scopes.size() == 1 && scopes.contains(UseScope.Default), s"$className uses $name in $scopes")

    val classesInActualSrc = analysisCallback.classNames(tempSrcFile).map(_._1)
    classesInActualSrc.map(className => className -> analysisCallback.usedNames(className)).toMap
  }

  /**
   * Extract used names from the last source file in `sources`.
   *
   * The previous source files are provided to successfully compile examples.
   * Only the names used in the last src file are returned.
   */
  def extractUsedNamesFromSrc(sources: String*): Map[String, Set[String]] = {
    val (srcFiles, analysisCallback) = compileSrcs(sources: _*)
    srcFiles
      .map { srcFile =>
        val classesInSrc = analysisCallback.classNames(srcFile).map(_._1)
        classesInSrc.map(className => className -> analysisCallback.usedNames(className)).toMap
      }
      .reduce(_ ++ _)
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
  def extractDependenciesFromSrcs(srcs: List[List[String]]): ExtractedClassDependencies = {
    val (_, testCallback) = compileSrcs(srcs)

    val memberRefDeps = testCallback.classDependencies collect {
      case (target, src, DependencyByMemberRef) => (src, target)
    }
    val inheritanceDeps = testCallback.classDependencies collect {
      case (target, src, DependencyByInheritance) => (src, target)
    }
    val localInheritanceDeps = testCallback.classDependencies collect {
      case (target, src, LocalDependencyByInheritance) => (src, target)
    }
    ExtractedClassDependencies.fromPairs(memberRefDeps, inheritanceDeps, localInheritanceDeps)
  }

  def extractDependenciesFromSrcs(srcs: String*): ExtractedClassDependencies = {
    extractDependenciesFromSrcs(List(srcs.toList))
  }

  /**
   * Compiles given source code snippets written to temporary files. Each snippet is
   * written to a separate temporary file.
   *
   * Snippets can be grouped to be compiled together in the same compiler run. This is
   * useful to compile macros, which cannot be used in the same compilation run that
   * defines them.
   *
   * The sequence of temporary files corresponding to passed snippets and analysis
   * callback is returned as a result.
   */
  def compileSrcs(groupedSrcs: List[List[String]]): (Seq[VirtualFile], TestCallback) = {
      val temp = IO.createTemporaryDirectory
      val analysisCallback = new TestCallback
      val testProgress = new TestCompileProgress
      val classesDir = new File(temp, "classes")
      classesDir.mkdir()

      val bridge = new CompilerBridge

      val files = for ((compilationUnit, unitId) <- groupedSrcs.zipWithIndex) yield {
        val srcFiles = compilationUnit.toSeq.zipWithIndex.map {
          (src, i) =>
            val fileName = s"Test-$unitId-$i.scala"
            prepareSrcFile(temp, fileName, src)
        }

        val virtualSrcFiles = srcFiles.toArray
        val classesDirPath = classesDir.getAbsolutePath.toString
        val output = new SingleOutput:
          def getOutputDirectory() = classesDir

        bridge.run(
          virtualSrcFiles,
          new TestDependencyChanges,
          Array("-Yforce-sbt-phases", "-classpath", classesDirPath, "-usejavacp", "-d", classesDirPath),
          output,
          analysisCallback,
          new TestReporter,
          testProgress,
          new TestLogger
        )

        srcFiles
      }
      (files.flatten.toSeq, analysisCallback)
  }

  def compileSrcs(srcs: String*): (Seq[VirtualFile], TestCallback) = {
    compileSrcs(List(srcs.toList))
  }

  private def prepareSrcFile(baseDir: File, fileName: String, src: String): VirtualFile = {
    val srcFile = new File(baseDir, fileName)
    IO.write(srcFile, src)
    new TestVirtualFile(srcFile.toPath)
  }
}

