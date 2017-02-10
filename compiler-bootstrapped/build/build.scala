import cbt._
class Build(val context: Context) extends Dotty{
  def compilerDirectory = projectDirectory ++ "/../compiler"
  override def dottyDependency = DirectoryDependency( compilerDirectory )
  override def sources = Seq( compilerDirectory ++ "/src/dotty" )
  def sbtV = "0.13.13"
  override def dependencies: Seq[Dependency] =
    Seq(
      DirectoryDependency( projectDirectory ++ "/../library-bootstrapped" ),
      DirectoryDependency( projectDirectory ++ "/../interfaces" )
    ) ++
    Resolver( mavenCentral ).bind(
      "me.d-d" % "scala-compiler" % "2.11.5-20160322-171045-e19b30b3cd",
      "com.typesafe.sbt" % "sbt-interface" % sbtV
    )
}
