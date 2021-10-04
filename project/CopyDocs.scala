import scala.io.{ Codec, Source }
import java.nio.file.{ FileSystems, Files, Path, StandardCopyOption }
import java.io.PrintStream
import collection.JavaConverters._


/**
 * Object used for copying docs from docs to docs-for-dotty-page. More explenation can be found and [readme](../docs/README.md)
 */
object CopyDocs {

  /**
   * Input directory from which we will take all the docs.
   */
  val inputDir = FileSystems.getDefault.getPath("docs")

  /**
   * Output directory. Most of the sources will be copied as they are, but some of the files will have headers and links processed.
   */
  val outputDir = FileSystems.getDefault.getPath("docs-for-dotty-page")

  implicit def stringToFun(s: String): MyParams => String = _ => s

  // Patterns, for convenience
  val titlePattern = "(?s)^---\n.*?title: ([^\n]*).*?---"
  val jekyllLinkPattern = """\{\% link _overviews/scala3-reference(.*) %\}"""
  val jekyllLinkSubstitution = "..$1"
  val jekyllLinkPattern2 = """\{\% link _overviews/scala3-scaladoc(.*) %\}"""
  val jekyllLinkSubstitution2 = ".$1"
  val localLinkPattern = """\((?!http|www)(.*).html\)"""
  val localLinkSubstitution = "($1.md)"

  case class MyParams(newPath: String)

  val commonTransformations: Set[(String, MyParams => String)] = Set(
    jekyllLinkPattern -> jekyllLinkSubstitution,
    jekyllLinkPattern2 -> jekyllLinkSubstitution2,
    localLinkPattern -> localLinkSubstitution,
  )

  /**
   * Structure for holding which transformations should be applied to which directories.
   * The outer map is holding morphism `directory prefix` -> `set of transformations`.
   * The inner set is a collection of pairs `regex pattern` -> `substitution value`.
   */
  val transformationMap: Map[String, Set[(String, MyParams => String)]] = Map(
    "docs/docs/usage/scaladoc/index.md" -> Set(
      ("""\{\{ site\.baseurl \}\}/resources/images/scala3/scaladoc/logo\.svg""" -> "images/scaladoc-logo.png"),
    ),

    "docs/docs/usage/scaladoc/site-versioning.md" -> Set(
      ("""/resources/images/scala3/scaladoc/nightly\.gif""" -> "images/scaladoc/nightly.gif"),
    ),

    "docs/docs/usage/scaladoc/search-engine.md" -> Set(
      ("""/resources/images/scala3/scaladoc/inkuire-1\.0\.0-M2_js_flatMap\.gif""" -> "images/scaladoc/inkuire-1.0.0-M2_js_flatMap.gif"),
    ),

    "docs/docs/reference/other-new-features/explicit-nulls.md" -> Set(
      ("""/resources/images/scala3/explicit-nulls/explicit-nulls-type-hierarchy\.png""" -> "images/explicit-nulls/explicit-nulls-type-hierarchy.png"),
    ),

    "docs/docs/reference/" -> (commonTransformations +
      (titlePattern -> ((p) => s"---\nlayout: doc-page\ntitle: $$1\nmovedTo: https://docs.scala-lang.org/scala3/reference/${p.newPath}.html\n---")),
    ),

    "docs/docs/usage/scaladoc/" -> (commonTransformations +
      (titlePattern -> s"---\ntitle: $$1\n---"),
    ),

    "docs/docs/usage/getting-started" -> (commonTransformations +
      (titlePattern -> "---\nlayout: doc-page\ntitle: Getting Started: Users\nmovedTo: https://docs.scala-lang.org/scala3/getting-started.html\n---"),
    ),

    "docs/docs/usage/tools-worksheets" -> (commonTransformations +
      (titlePattern -> "---\nlayout: doc-page\ntitle: \"Worksheet mode with Dotty IDE\"\nmovedTo: https://docs.scala-lang.org/scala3/book/tools-worksheets.html\n---") +
      ("""/resources/images/scala3-book/intellij-worksheet\.png""" -> "images/worksheets/intellij-worksheet.png") +
      ("""/resources/images/scala3-book/metals-worksheet\.png""" -> "images/worksheets/metals-worksheet.png")
    ),

    "docs/docs/resources/talks" -> (commonTransformations +
      (titlePattern -> "---\nlayout: doc-page\ntitle: Talks\nmovedTo: https://docs.scala-lang.org/scala3/talks.html\n---")
    )
  )

  def copyDocs() = {
    def copyFile(path: Path): Unit = {
      val newPath = outputDir.resolve(inputDir.relativize(path))
      Files.createDirectories(newPath.getParent())

      path.toString match {
        case s if s.startsWith("docs/docs/") =>
          val inputStream = Source.fromFile(path.toFile)(Codec.UTF8)
          val fileContent = inputStream.getLines().mkString("\n")

          new PrintStream(newPath.toFile) {
            val patterns = transformationMap.filter { case (k, v) => path.toString.startsWith(k) }.flatMap(_._2)
            val params = MyParams(newPath = s.stripPrefix("docs/docs/reference/").stripSuffix(".md"))
            val transformed = patterns.foldLeft(fileContent) { case (res, (pattern, substitution)) => res.replaceAll(pattern, substitution(params)) }
            write(transformed.getBytes("UTF8"))
          }
        case s =>
          Files.copy(path, newPath, StandardCopyOption.REPLACE_EXISTING);
      }
    }
    Files.walk(inputDir).iterator().asScala.filter(Files.isRegularFile(_)).foreach(copyFile)
  }
}
