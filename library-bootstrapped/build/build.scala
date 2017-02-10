import cbt._
class Build(val context: Context) extends Dotty{
  override def dottyDependency = DirectoryDependency( projectDirectory ++ "/../compiler" )
  override def sources = Seq( "dotty", "scala", "scalaShadowing" ).map(
    f => projectDirectory ++ ( "/../library/src/" + f )
  )
  def scalaV = "2.11.5"
  override def dependencies =
    Resolver( mavenCentral, sonatypeReleases ).bind(
      "org.scala-lang" % "scala-reflect" % scalaV,
      "org.scala-lang" % "scala-library" % scalaV
    )
}
