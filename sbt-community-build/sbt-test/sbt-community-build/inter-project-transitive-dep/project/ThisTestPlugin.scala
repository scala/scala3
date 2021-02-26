import sbt._
import Keys._

object ThisTestPlugin extends AutoPlugin {
  override def requires = plugins.IvyPlugin
  override def trigger = allRequirements

  val thisTestIvyHome = settingKey[File]("Ivy home directory for artifacts published by this test")
  val thisTestResolver = settingKey[Resolver]("Resolver for artifacts published by this test")
  val deleteDepsFile = taskKey[Unit]("Deletes the dotty-community-build-deps dependency tracking file")

  override val projectSettings = Seq(
    publishLocalConfiguration := publishLocalConfiguration.value.withResolverName("this-test")
  )

  override val buildSettings = defaultThisTestSettings ++ Seq(
    resolvers += thisTestResolver.value
  )

  def defaultThisTestSettings: Seq[Setting[_]] = {
    Seq(
      thisTestIvyHome := (LocalRootProject / target).value / "ivy-cache",
      thisTestResolver := Resolver.file("this-test", thisTestIvyHome.value / "local")(Resolver.ivyStylePatterns),
      deleteDepsFile := IO.delete(file(sys.props("dotty.communitybuild.dir")) / "dotty-community-build-deps"),
    )
  }

  object autoImport {
    def onlyThisTestResolverSettings: Seq[Setting[_]] = Seq(
      externalResolvers := thisTestResolver.value :: Nil
    )
  }
}
