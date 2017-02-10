import cbt._
class Build(val context: Context) extends BaseBuild{
  override def sources = Seq(  projectDirectory ++ "/src/dotty" )
  def sbtV = "0.13.13"
  override def dependencies =
    Seq(
      DirectoryDependency( projectDirectory ++ "/../library" ),
      DirectoryDependency( projectDirectory ++ "/../interfaces" )
    ) ++
    Resolver( mavenCentral ).bind(
      "me.d-d" % "scala-compiler" % "2.11.5-20160322-171045-e19b30b3cd",
      "com.typesafe.sbt" % "sbt-interface" % sbtV
    )
}
