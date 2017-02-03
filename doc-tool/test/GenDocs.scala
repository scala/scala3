package dotty.tools
package dottydoc

object Files {
  type JFile = java.io.File

  implicit class ToUrl(val f: JFile) extends AnyVal {
    def getUrl = f.toURI.toURL
  }
}

trait LocalResources extends DocDriver {
  import Files._
  import dotty.Jars

  def getFiles(file: JFile): Array[JFile] =
    if (file.isDirectory) file.listFiles.flatMap(getFiles)
    else if (file.getAbsolutePath.endsWith(".scala")) Array(file)
    else Array()

  def withClasspath(files: Array[String]) =
    "-siteroot" +: "../docs" +:
    "-project" +: "Dotty" +:
    "-language:Scala2" +:
    "-classpath" +:
    (Jars.dottyLib + ":" + Jars.dottyInterfaces) +:
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
