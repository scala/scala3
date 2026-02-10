package scala3build

import build_.package_ as build
import mill.*
import mill.scalalib.*

trait Scala3Module extends Scala3JavaModule with ScalaModule { outer =>
  def buildType: BuildType = BuildType.Final

  def resolutionParams = Task.Anon {
    val baseParams = super.resolutionParams()
    baseParams.withForceVersion0(
      baseParams.forceVersion0.filter(_._1.organization.value != "org.scala-lang")
    )
  }

  def scalaVersion =
    if (buildType.isBootstrapping) Versions.referenceVersion
    else Versions.dottyVersion

  def scalaLibraryMvnDeps = Nil
  def scalaCompilerClasspath =
    if (buildType.isBootstrapping)
      Task {
        defaultResolver().classpath(
          Seq(mvn"org.scala-lang:scala3-compiler_3:${Versions.referenceVersion}")
        )
      }
    else
      build.compiler(BuildType.Bootstrapping).runClasspathAsJars

  def scalaCompilerBridge =
    if (buildType.isBootstrapping)
      Task(None)
    else
      Task {
        Some(build.`sbt-bridge`(BuildType.Bootstrapping).jar())
      }

  def scalacOptions = super.scalacOptions() ++ Seq(
    "-feature",
    "-deprecation",
    "-unchecked",
    //"-Wconf:cat=deprecation&msg=Unsafe:s",    // example usage
    //"-Wunused:all",
    //"-rewrite", // requires -Werror:false since no rewrites are applied with errors
    "-encoding", "UTF8",
    "-language:implicitConversions",
    "--java-output-version", Versions.minimumJVMVersion
  )

  trait Scala3Tests extends ScalaTests with Scala3JavaTests { self =>

    def resolutionParams = Task.Anon {
      val baseParams = super.resolutionParams()
      baseParams.withForceVersion0(
        baseParams.forceVersion0.filter(_._1.organization.value != "org.scala-lang")
      )
    }

    def scalaLibraryMvnDeps = outer.scalaLibraryMvnDeps
    def scalaCompilerClasspath = outer.scalaCompilerClasspath

    def testArgsDefault = Task {
      val extraArgs = if (buildType.isBootstrapping) Seq("--exclude-categories=dotty.BootstrappedOnlyTests") else Nil
      super.testArgsDefault() ++ extraArgs
    }
  }

  def enableBsp = buildType.isBootstrapping || Scala3Module.enableBspForFinalModules
}

object Scala3Module {
  def enableBspForFinalModules = false
}
