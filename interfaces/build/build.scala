import cbt._
class Build(val context: Context) extends BaseBuild{
  override def sources = Seq(  projectDirectory ++ "/src/dotty" )
}
