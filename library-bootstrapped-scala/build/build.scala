import cbt._
class Build(val context: Context) extends Dotty{
  override def dottyDependency = DirectoryDependency( projectDirectory ++ "/../compiler" )
  override def sources = Seq( "dotty", "scala", "scalaShadowing" ).map(
    f => projectDirectory ++ ( "/../library/src/" + f )
  )
  override def dependencies =
    // copy dependencies from maven ;)
    Resolver(mavenCentral).bindOne(
      MavenDependency("ch.epfl.lamp","dotty-library_2.11",Dotty.version)
    ).dependencies
}
