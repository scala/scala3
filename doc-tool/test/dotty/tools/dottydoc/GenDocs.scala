package dotty.tools
package dottydoc

import vulpix.TestConfiguration

object Files {
  type JFile = java.io.File

  implicit class ToUrl(val f: JFile) extends AnyVal {
    def getUrl = f.toURI.toURL
  }
}

trait LocalResources extends DocDriver {
  import Files._

  def getFiles(file: JFile): Array[JFile] =
    if (file.isDirectory) file.listFiles.flatMap(getFiles)
    else if (file.getAbsolutePath.endsWith(".scala")) Array(file)
    else Array()

  def withClasspath(files: Array[String]) =
    "-siteroot" +: "../docs" +:
    "-project" +: "Dotty" +:
    "-language:Scala2Compat" +:
    "-classpath" +:
    TestConfiguration.basicClasspath +:
    files
}

object GenDottyDocs extends LocalResources {
  import Files._

  val dottyFiles = new JFile("../compiler/src/dotty").listFiles.flatMap(getFiles).map(_.getAbsolutePath)

  override def main(args: Array[String]): Unit =
    super.main(withClasspath(dottyFiles))
}
