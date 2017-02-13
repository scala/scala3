import cbt._
class Build(val context: Context) extends PackageJars{
  override def defaultScalaVersion = "2.11.5"
  override def sources = Seq(  projectDirectory ++ "/src/dotty" )
  override def dependencies = Seq()

  def groupId: String = "ch.epfl.lamp"
  def name: String = "dotty-interfaces"
  def version: String = Dotty.version
}
