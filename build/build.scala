import cbt._
class Build(val context: Context) extends Dotty with PackageJars{
  override def dottyDependency = DirectoryDependency( projectDirectory ++ "/compiler-bootstrapped" )
  override def sources = context.args.map(f => projectDirectory ++ ("/"++f))
  override def dependencies = Seq(dottyDependency)

  def groupId: String = "ch.epfl.lamp"
  def name: String = "dotty"
  def version: String = Dotty.version
}
