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

  val jekyllLinkPattern = """\{\% link _overviews/scala3-reference(.*) %\}"""
  val jekyllLinkSubstitution = "..$1"
  val jekyllLinkPattern2 = """\{\% link _overviews/scala3-scaladoc(.*) %\}"""
  val jekyllLinkSubstitution2 = ".$1"
  val localLinkPattern = """\((?!http|www)(.*).html\)"""
  val localLinkSubstitution = "($1.md)"

  val commonTransformations: Map[String, String] = Map(
    jekyllLinkPattern -> jekyllLinkSubstitution,
    jekyllLinkPattern2 -> jekyllLinkSubstitution2,
    localLinkPattern -> localLinkSubstitution,
  )

  /**
   * Structure for holding which transformations should be applied to which directories.
   * The outer map is holding morphism `directory prefix` -> `List of transformations`.
   * The inner list is a collection of pairs `regex pattern` -> `substitution value`.
   */
  val transformationMap: Map[String, Map[String, String]] = Map(
    "docs/docs/usage/scaladoc/index.md" -> Map(
      ("""\{\{ site\.baseurl \}\}/resources/images/scala3/scaladoc/logo\.svg""" -> "images/scaladoc_logo.svg"),
    ),

    "docs/docs/usage/scaladoc/site-versioning.md" -> Map(
      ("""/resources/images/scala3/scaladoc/nightly\.gif""" -> "images/scaladoc/nightly.gif"),
    ),

    "docs/docs/usage/scaladoc/search-engine.md" -> Map(
      ("""/resources/images/scala3/scaladoc/inkuire-1\.0\.0-M2_js_flatMap\.gif""" -> "images/scaladoc/inkuire-1.0.0-M2_js_flatMap.gif"),
    ),

    "docs/docs/reference/other-new-features/explicit-nulls.md" -> Map(
      ("""/resources/images/scala3/explicit-nulls/explicit-nulls-type-hierarchy\.png""" -> "images/explicit-nulls/explicit-nulls-type-hierarchy.png"),
    ),

    "docs/docs/reference/" -> commonTransformations,

    "docs/docs/usage/scaladoc/" -> commonTransformations
  )

  def copyDocs() = {
    def copyFile(path: Path): Unit = {
      val newPath = outputDir.resolve(inputDir.relativize(path))
      Files.createDirectories(newPath.getParent())

      path.toString match {
        case s if s.startsWith("docs/docs/") && s.endsWith(".md") =>
          val inputStream = Source.fromFile(path.toFile)(Codec.UTF8)
          val fileContent = inputStream.getLines().mkString("\n")

          new PrintStream(newPath.toFile) {
            val patterns = transformationMap.filter { case (k, v) => path.toString.startsWith(k) }.flatMap(_._2)
            val _ :: frontMatter :: actualContent :: Nil = fileContent.split("---", 3).toList
            write("---".getBytes("UTF8"))
            val frontMatterSplitted = frontMatter.split("\n(?=[^\\s])")
            val frontMatterUpdated = List(
              Some("layout: doc-page"),
              frontMatterSplitted.find(_.startsWith("title")),
              frontMatterSplitted.find(_.startsWith("redirectFrom")),
              if (s.startsWith("docs/docs/reference/")) Some(s"movedTo: https://docs.scala-lang.org/scala3/reference/${s.stripPrefix("docs/docs/reference/").stripSuffix(".md")}.html") else None
            ).flatten.mkString("\n", "\n", "\n")
            write(frontMatterUpdated.getBytes("UTF8"))
            write("---\n".getBytes("UTF8"))
            val transformed = patterns.foldLeft(actualContent) { case (res, (pattern, substitution)) => res.replaceAll(pattern, substitution) }
            write(transformed.getBytes("UTF8"))
          }
        case s =>
          Files.copy(path, newPath, StandardCopyOption.REPLACE_EXISTING);
      }
    }
    Files.walk(inputDir).iterator().asScala.filter(Files.isRegularFile(_)).foreach(copyFile)
  }
}
