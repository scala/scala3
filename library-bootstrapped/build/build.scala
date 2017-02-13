import cbt._
class Build(val context: Context) extends PackageJars{
  override def sources = Seq(
    projectDirectory ++ "/../library/src/scala"
  )
  override def sourceFileFilter(file: java.io.File): Boolean = file.toString.endsWith(".java")
  /**
  This seperate build is needed because dotc does not compile .java files.
  No longer needed as soon as cbt's dotty support uses zinc.
  */
  val libraryBootstrappedScala = DirectoryDependency(
    projectDirectory ++ "/../library-bootstrapped-scala"
  ).build
  override def dependencies = libraryBootstrappedScala.dependencies
  override def exportedClasspath = libraryBootstrappedScala.exportedClasspath ++ super.exportedClasspath

  def groupId: String = "ch.epfl.lamp"
  def name: String = "dotty-library"
  def version: String = Dotty.version
}
