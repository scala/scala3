package dotty.tools
package dottydoc

object Files {
  type JFile = java.io.File

  implicit class ToUrl(val f: JFile) extends AnyVal {
    def getUrl = f.toURI.toURL
  }
}

trait LocalResources extends api.scala.Dottydoc {
  import Files._

  def getFiles(file: JFile): Array[JFile] =
    if (file.isDirectory) file.listFiles.flatMap(getFiles)
    else if (file.getAbsolutePath.endsWith(".scala")) Array(file)
    else Array()

  def withClasspath(files: Array[String]) =
    "-siteroot" +: "../docs" +:
    "-project" +: "Dotty" +:
    "-language:Scala2" +:
    "-classpath" +: "../library/target/scala-2.11/dotty-library_2.11-0.1-SNAPSHOT.jar:../interfaces/target/dotty-interfaces-0.1-SNAPSHOT.jar" +:
    files
}

object GenCollections extends LocalResources {
  import Files._

  val collections = TestWhitelistedCollections.files

  override def main(args: Array[String]): Unit =
    super.main(withClasspath(collections.toArray))
}

object GenDottyDocs extends LocalResources {
  import Files._

  val dottyFiles = new JFile("../compiler/src/dotty").listFiles.flatMap(getFiles).map(_.getAbsolutePath)

  override def main(args: Array[String]): Unit =
    super.main(withClasspath(dottyFiles))
}
