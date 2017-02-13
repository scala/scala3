import cbt._
class Build(val context: Context) extends BaseBuild{
  override def sources = Seq(  projectDirectory ++ "/src/dotty" )
  override def dependencies =
    Seq(
      DirectoryDependency( projectDirectory ++ "/../library" ),
      DirectoryDependency( projectDirectory ++ "/../interfaces" )
    ) ++
    // copy dependencies from maven ;), but ignore interfaces and library
    Resolver(mavenCentral).bindOne(
      MavenDependency("ch.epfl.lamp","dotty-compiler_2.11",Dotty.version)
    ).dependencies.filter{
      case d: BoundMavenDependency
        if d.mavenDependency.groupId == "ch.epfl.lamp"
        && Set( "dotty-interfaces","dotty-library_2.11" ).contains( d.mavenDependency.artifactId )
        => false
      case other => true
    }
}
