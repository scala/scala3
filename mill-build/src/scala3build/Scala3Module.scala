package scala3build

import build_.package_ as build
import mill.*
import mill.api.BuildCtx
import mill.scalalib.*

import scala.annotation.tailrec

/**
 * Tasks for Scala modules in the Scala 3 build.
 *
 * Beyond what [[Scala3JavaModule]] adds, this deals with setting up
 * the library, compiler, compiler bridge, and scaladoc, when non-bootstrapped and
 * for bootstrapped modules. This also sets some shared scalac options.
 */
trait Scala3Module extends Scala3JavaModule, ScalaModule:
  outer =>
  def mode: Mode = Mode.Bootstrapped

  def sourcesFolders = super.sourcesFolders ++ Seq(
    if (mode == Mode.NonBootstrapped) "src-non-bootstrapped" else "src-bootstrapped"
  )

  def resolutionParams = Task.Anon {
    val baseParams = super.resolutionParams()
    baseParams.withForceVersion0(
      baseParams.forceVersion0.filter(_._1.organization.value != "org.scala-lang")
    )
  }

  def scalaVersion =
    if (mode == Mode.NonBootstrapped) Versions.referenceVersion
    else Versions.dottyVersion

  def scalaLibraryMvnDeps = Nil
  def scalaCompilerClasspath =
    if (mode == Mode.NonBootstrapped)
      Task {
        defaultResolver().classpath(
          Seq(mvn"org.scala-lang:scala3-compiler_3:${Versions.referenceVersion}")
        )
      }
    else
      build.compiler.`non-bootstrapped`.runClasspathAsJars

  def scalaCompilerBridge =
    if (mode == Mode.NonBootstrapped)
      Task(None)
    else
      Task {
        Some(build.`sbt-bridge`.`non-bootstrapped`.jar())
      }

  def scalaDocClasspath =
    if (mode == Mode.NonBootstrapped)
      super.scalaDocClasspath
    else
      // Use the *bootstrapped* scaladoc to build all bootstrapped (that is, published) modules' scaladoc,
      // even the one of the bootstrapped scaladoc itself (it builds its own scaladoc)
      build.scaladoc.runClasspathAsJars

  def scalacOptions = super.scalacOptions() ++ Seq(
    "-feature",
    "-deprecation",
    "-unchecked",
    //"-Wconf:cat=deprecation&msg=Unsafe:s",    // example usage
    //"-Wunused:all",
    //"-rewrite", // requires -Werror:false since no rewrites are applied with errors
    "-encoding", "UTF8",
    "-language:implicitConversions",
    "--java-output-version", Versions.minimumJVMVersion,
    "-sourceroot", BuildCtx.workspaceRoot.toString
  )

  def scalaDocOptions = Task {
    val baseOptions = super.scalaDocOptions()
    val filteredBaseOptions = {
      @tailrec
      def helper(opts: Seq[String]): Seq[String] = {
        val sourcepathIdx = opts.indexOf("-sourcepath")
        if (sourcepathIdx >= 0)
          helper(opts.take(sourcepathIdx) ++ opts.drop(sourcepathIdx + 2))
        else
          opts
      }
      helper(baseOptions)
    }
    val extraOptions =
      if (mode == Mode.Bootstrapped) {
        val rawExtraOptions = ScaladocOptions.scalacOptionsDocSettings()
        // Make -project-logo arg an absolute path, as scaladoc isn't going to run
        // from the root of the project with Mill
        val projLogoIdx = rawExtraOptions.indexOf("-project-logo")
        if (projLogoIdx >= 0)
          rawExtraOptions.take(projLogoIdx + 1) ++
            Seq(os.Path(rawExtraOptions(projLogoIdx + 1), BuildCtx.workspaceRoot).toString) ++
            rawExtraOptions.drop(projLogoIdx + 2)
        else
           rawExtraOptions
      }
      else
        Nil
    filteredBaseOptions ++ extraOptions
  }

  trait Scala3Tests extends ScalaTests, Scala3JavaTests:
    // Future versions of Mill should handle these two
    def scalaLibraryMvnDeps = outer.scalaLibraryMvnDeps
    def scalaCompilerClasspath = outer.scalaCompilerClasspath

  def enableBsp = super.enableBsp &&
    (mode == Mode.NonBootstrapped || Scala3Module.enableBspForBootstrappedModules)

object Scala3Module:
  def enableBspForBootstrappedModules = false
