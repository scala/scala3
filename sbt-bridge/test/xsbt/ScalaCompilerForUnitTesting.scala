/** Adapted from https://github.com/sbt/sbt/blob/0.13/compile/interface/src/test/scala/xsbt/ScalaCompilerForUnitTesting.scala */
package xsbt

import dotty.tools.xsbt.CompilerBridge
import sbt.io.IO
import xsbti.*
import xsbti.api.ClassLike
import xsbti.api.DependencyContext.*
import xsbti.compile.SingleOutput

import java.io.File
import java.nio.file.Path

import TestCallback.ExtractedClassDependencies

case class CompileOutput(srcFiles: Seq[VirtualFileRef], classesOutput: Path, analysis: TestCallback, progress: TestCompileProgress)

/**
 * Provides common functionality needed for unit tests that require compiling
 * source code using Scala compiler.
 */
class ScalaCompilerForUnitTesting {

  def extractEnteredPhases(srcs: String*): Seq[List[String]] = {
    val output = compileSrcs(srcs*)
    val run = output.progress.runs.head
    output.srcFiles.map(src => run.unitPhases(src.id))
  }

  def extractTotal(srcs: String*)(extraSourcePath: String*): Int =
    compileSrcs(List(srcs.toList), extraSourcePath.toList).progress.runs.head.total

  def extractProgressPhases(srcs: String*): List[String] =
    compileSrcs(srcs*).progress.runs.head.phases

  /**
   * Compiles given source code using Scala compiler and returns API representation
   * extracted by ExtractAPI class.
   */
  def extractApiFromSrc(src: String): Seq[ClassLike] = {
    val output = compileSrcs(src)
    output.analysis.apis(output.srcFiles.head)
  }

  /**
   * Compiles given source code using Scala compiler and returns API representation
   * extracted by ExtractAPI class.
   */
  def extractApisFromSrcs(srcs: List[String]*): Seq[Seq[ClassLike]] = {
    val output = compileSrcs(srcs.toList)
    output.srcFiles.map(output.analysis.apis)
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
    val output = compileSrcs(definitionSrc, actualSrc)
    val analysis = output.analysis

    if (assertDefaultScope) for {
      (className, used) <- analysis.usedNamesAndScopes
      analysis.TestUsedName(name, scopes) <- used
    } assert(scopes.size() == 1 && scopes.contains(UseScope.Default), s"$className uses $name in $scopes")

    val classesInActualSrc = analysis.classNames(output.srcFiles.head).map(_._1)
    classesInActualSrc.map(className => className -> analysis.usedNames(className)).toMap
  }

  /**
   * Extract used names from the last source file in `sources`.
   *
   * The previous source files are provided to successfully compile examples.
   * Only the names used in the last src file are returned.
   */
  def extractUsedNamesFromSrc(sources: String*): Map[String, Set[String]] = {
    val output = compileSrcs(sources*)
    output.srcFiles
      .map { srcFile =>
        val classesInSrc = output.analysis.classNames(srcFile).map(_._1)
        classesInSrc.map(className => className -> output.analysis.usedNames(className)).toMap
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
    val analysis = compileSrcs(srcs).analysis

    val memberRefDeps = analysis.classDependencies collect {
      case (target, src, DependencyByMemberRef) => (src, target)
    }
    val inheritanceDeps = analysis.classDependencies collect {
      case (target, src, DependencyByInheritance) => (src, target)
    }
    val localInheritanceDeps = analysis.classDependencies collect {
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
  def compileSrcs(groupedSrcs: List[List[String]], sourcePath: List[String] = Nil, compileToJar: Boolean = false): CompileOutput = {
      val temp = IO.createTemporaryDirectory
      val analysisCallback = new TestCallback
      val testProgress = new TestCompileProgress
      val classesOutput =
        if (compileToJar) {
          val jar = new File(temp, "classes.jar")
          jar.createNewFile()
          jar
        } else {
          val dir = new File(temp, "classes")
          dir.mkdir()
          dir
        }

      val bridge = new CompilerBridge

      val files = for ((compilationUnits, unitId) <- groupedSrcs.zipWithIndex) yield {
        val extraFiles = sourcePath.toSeq.zipWithIndex.map {
          case (src, i) =>
            val fileName = s"Extra-$unitId-$i.scala"
            prepareSrcFile(temp, fileName, src)
        }
        val srcFiles = compilationUnits.toSeq.zipWithIndex.map {
          (src, i) =>
            val fileName = s"Test-$unitId-$i.scala"
            prepareSrcFile(temp, fileName, src)
        }

        val virtualSrcFiles = srcFiles.toArray
        val classesOutputPath = classesOutput.getAbsolutePath()
        val output = new SingleOutput:
          def getOutputDirectory() = classesOutput

        val maybeSourcePath = if extraFiles.isEmpty then Nil else List("-sourcepath", temp.getAbsolutePath.toString)

        bridge.run(
          virtualSrcFiles,
          new TestDependencyChanges,
          Array("-Yforce-sbt-phases", "-classpath", classesOutputPath, "-usejavacp", "-d", classesOutputPath) ++ maybeSourcePath,
          output,
          analysisCallback,
          new TestReporter,
          testProgress,
          new TestLogger
        )

        testProgress.completeRun()

        srcFiles
      }
      CompileOutput(files.flatten.toSeq, classesOutput.toPath, analysisCallback, testProgress)
  }

  def compileSrcs(srcs: String*): CompileOutput = {
    compileSrcs(List(srcs.toList))
  }

  def compileSrcsToJar(srcs: String*): CompileOutput =
    compileSrcs(List(srcs.toList), compileToJar = true)

  private def prepareSrcFile(baseDir: File, fileName: String, src: String): VirtualFile = {
    val srcFile = new File(baseDir, fileName)
    IO.write(srcFile, src)
    new TestVirtualFile(srcFile.toPath)
  }
}

