package scala3build

import com.github.lolgab.mill.mima.Mima
import mill.*
import mill.scalalib.*
import mill.scalalib.publish.*

/**
 * Tasks for published Java / Scala modules in the Scala 3 build
 *
 * This sets up publishing, JVM version and --release options, test framework, and more.
 */
trait Scala3JavaModule extends JavaModule, PublishModule:
  def artifactName = "scala3-" + super.artifactName()

  def jvmId = "17" // force external zinc worker

  def pomSettings = PomSettings(
    description = artifactName(),
    organization = "org.scala-lang",
    url = "https://github.com/scala/scala3",
    licenses = Seq(License.`Apache-2.0`),
    versionControl = VersionControl.github("scala", "scala3"),
    developers = Seq(Developer("scala", "The Scala Team", "https://scala-lang.org", email = "security@scala-lang.org"))
  )
  def publishVersion = Versions.dottyVersion

  def javacOptions = super.javacOptions() ++ Seq(
    "--release", Versions.minimumJVMVersion
  )

  def manifest = super.manifest().add(
    "Automatic-Module-Name" ->
      s"${pomSettings().organization.replaceAll("-", ".")}.${artifactName().replaceAll("-", ".")}"
  )

  def runClasspathAsJars: T[Seq[PathRef]] = Task {
    resolvedRunMvnDeps().toSeq ++
      transitiveJars() ++
      Seq(jar())
  }

  def runClasspath = runClasspathAsJars

  def testSources = Task.Source("test")

  trait Scala3JavaTests extends JavaTests, TestModule:
    def artifactName = "scala3-" + super.artifactName()
    def sources = Task {
      Seq(testSources())
    }
    def testParallelism = false
    def testSandboxWorkingDir = false

    // FIXME Not using Mill's TestModule.Junit4 as it forces junit-interface even on Scala.js, where
    // it clashes with the Scala.js junit4 substitute
    def testFramework = "com.novocode.junit.JUnitFramework"
    def mandatoryMvnDeps = super.mandatoryMvnDeps() ++ Seq(
      mvn"com.github.sbt:junit-interface:0.13.3"
    )

  def isCoreModule: Boolean = false
  def enableBsp = isCoreModule || Scala3JavaModule.enableBspForNonCoreModules
  def skipIdea = enableBsp

object Scala3JavaModule:
  def enableBspForNonCoreModules: Boolean = true
  def enableBspForScalaJsModules: Boolean = false
