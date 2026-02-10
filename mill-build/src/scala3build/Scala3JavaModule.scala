package scala3build

import com.github.lolgab.mill.mima.Mima
import mill.*
import mill.scalalib.*
import mill.scalalib.publish.*

trait Scala3JavaModule extends JavaModule with PublishModule with Mima {
  trait Scala3JavaTests extends JavaTests with TestModule with TestModule.Junit4 {
    def testSources: T[Seq[PathRef]]
    def sources = testSources
    def testParallelism = false

    def mvnDeps = super.mvnDeps() ++ Seq(
      mvn"com.github.sbt:junit-interface:0.13.3"
    )
  }

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
}
