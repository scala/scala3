import java.nio.file.{ FileSystems, Files, Path }
import java.io.PrintStream
import scala.io.{ Codec, Source }
import scala.collection.JavaConverters._

val inputDir = FileSystems.getDefault.getPath("docs-for-scalalang")
val outputDir = FileSystems.getDefault.getPath("docs")

def copyFile(path: Path): Unit =
  val inputStream = Source.fromFile(path.toFile)(Codec.UTF8)
  val fileContent = inputStream.getLines().mkString("\n")

  val newPath = outputDir.resolve(inputDir.relativize(path))
  Files.createDirectories(newPath.getParent())

  new PrintStream(newPath.toFile):

    val titlePattern = "(?s)^---\n.*title: (\".*\").*---"
    val reference = raw".*/reference/.*".r
    val usageScaladoc = raw".*/usage/scaladoc/.*".r
    val transformed = newPath.toString match
      case reference(_*) => fileContent.replaceAll(
          titlePattern,
          s"---\nlayout: doc-page\ntitle: $$1\nmovedTo: https://docs.scala-lang.org/scala3/reference/contextual/${newPath.getFileName.toString.stripSuffix(".md")}.html\n---"
        )
      case usageScaladoc(_*) => fileContent.replaceAll(
          titlePattern,
          s"---\ntitle: $$1\n---"
        )
      case _ =>
        fileContent


    val jekyllLinkPattern = """\(\{\% link _scala3-reference(.*) %\}\)"""
    val jekyllLinkSubstitution = "(..$1)"
    val localLinkPattern = """\((?!http|www)(.*).html\)"""
    val localLinkSubstitution = "($1.md)"
    val transformedWithLinks = transformed.replaceAll(
      jekyllLinkPattern,
      jekyllLinkSubstitution
    ).replaceAll(
      localLinkPattern,
      localLinkSubstitution
    )

    write(transformedWithLinks.getBytes("UTF8"))

def main(args: Array[String]): Unit =
  Files.walk(inputDir).iterator().asScala.filter(Files.isRegularFile(_)).foreach(copyFile)
