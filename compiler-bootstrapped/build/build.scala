import cbt._
class Build(val context: Context) extends Dotty with PackageJars{
  override def dottyDependency = DirectoryDependency( projectDirectory ++ "/../compiler" )
  override def sources = Seq( projectDirectory ++ "/../compiler/src/dotty" )

  override def dependencies: Seq[Dependency] =
    Seq(
      // this currently leads to an exception NoClassDefFoundError: scala/Product0$class
      // when trying to use this compiler. Changing to ../library makes it work.
      DirectoryDependency( projectDirectory ++ "/../library-bootstrapped" ),
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

  def groupId: String = "ch.epfl.lamp"
  def name: String = "dotty-compiler"
  def version: String = Dotty.version
}
