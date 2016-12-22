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

  val template = new JFile(
    sys.env.get("DOC_TEMPLATE").getOrElse("../../dottydoc-client/resources/template.html")
  )
  val resources = new JFile(
    sys.env.get("DOC_RESOURCES").getOrElse("../../dottydoc-client/resources/")
  ).listFiles

  assert(template.exists, "please specify a template.html file using DOC_TEMPLATE env var")
  assert(resources.forall(_.exists), "please specify a resource dir using DOC_RESOURCES env var")

  def index(files: Array[String]) = createIndex(
    "-language:Scala2" +: "-classpath" +: "../library/target/scala-2.11/dotty-library_2.11-0.1-SNAPSHOT.jar:../interfaces/target/dotty-interfaces-0.1-SNAPSHOT.jar" +: files
  )
}

object GenCollections extends LocalResources {
  import Files._

  val collections = TestWhitelistedCollections.files

  override def main(args: Array[String]): Unit = buildDocs(
    "../local/docs",
    template.getUrl,
    resources.map(_.getUrl).toList,
    index(collections.toArray)
  )
}

object GenDottyDocs extends LocalResources {
  import Files._

  def getFiles(file: JFile): Array[JFile] =
    if (file.isDirectory) file.listFiles.flatMap(getFiles)
    else if (file.getAbsolutePath.endsWith(".scala")) Array(file)
    else Array()

  val dottyFiles = new JFile("../compiler/src/dotty").listFiles.flatMap(getFiles).map(_.getAbsolutePath)

  override def main(args: Array[String]): Unit = buildDocs(
    "../local/docs",
    template.getUrl,
    resources.map(_.getUrl).toList,
    index(dottyFiles)
  )
}
