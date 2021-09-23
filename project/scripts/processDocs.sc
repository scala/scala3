import java.nio.file.{ FileSystems, Files, Path, StandardCopyOption }
import java.io.PrintStream
import scala.io.{ Codec, Source }
import scala.collection.JavaConverters._

val inputDir = FileSystems.getDefault.getPath("docs")
val outputDir = FileSystems.getDefault.getPath("docs-for-dotty-page")

def copyFile(path: Path): Unit =
  val newPath = outputDir.resolve(inputDir.relativize(path))
  Files.createDirectories(newPath.getParent())

  path.toString match
    case s if s.startsWith("docs/docs/") =>
      val inputStream = Source.fromFile(path.toFile)(Codec.UTF8)
      val fileContent = inputStream.getLines().mkString("\n")

      new PrintStream(newPath.toFile):

        // Patterns
        val titlePattern = "(?s)^---\n.*title: (\".*\").*---"

        val jekyllLinkPattern = """\{\% link _overviews/scala3-scaladoc(.*) %\}"""
        val jekyllLinkSubstitution = "..$1"
        val jekyllLinkPattern2 = """\{\% link |_overviews/scala3-scaladoc/(.*) %\}"""
        val jekyllLinkSubstitution2 = "$1"
        val localLinkPattern = """\((?!http|www)(.*).html\)"""
        val localLinkSubstitution = "($1.md)"

        // Prefixes
        val reference = raw"docs/docs/reference/.*".r
        val usageScaladoc = raw"docs/docs/usage/scaladoc/.*".r

        val patterns = path.toString match
          case reference(_*) => Map(
            titlePattern -> s"---\nlayout: doc-page\ntitle: $$1\nmovedTo: https://docs.scala-lang.org/scala3/reference/contextual/${newPath.getFileName.toString.stripSuffix(".md")}.html\n---",
            jekyllLinkPattern -> jekyllLinkSubstitution,
            jekyllLinkPattern2 -> jekyllLinkSubstitution2,
            localLinkPattern -> localLinkSubstitution,
          )
          case usageScaladoc(_*) => Map(
            titlePattern -> s"---\ntitle: $$1\n---",
            jekyllLinkPattern -> jekyllLinkSubstitution,
            jekyllLinkPattern2 -> jekyllLinkSubstitution2,
            localLinkPattern -> localLinkSubstitution,
          )
          case _ =>
            Map.empty

        val transformed = patterns.foldLeft(fileContent){ case (res, (pattern, substitution)) => res.replaceAll(pattern, substitution) }
        write(transformed.getBytes("UTF8"))

    case s =>
      Files.copy(path, newPath, StandardCopyOption.REPLACE_EXISTING);

def main(args: Array[String]): Unit =
  Files.walk(inputDir).iterator().asScala.filter(Files.isRegularFile(_)).foreach(copyFile)
