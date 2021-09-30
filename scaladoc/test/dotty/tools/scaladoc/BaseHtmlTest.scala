package dotty.tools.scaladoc

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import org.junit.Test
import org.junit.Assert._
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import java.nio.charset.Charset
import dotty.tools.scaladoc.test.BuildInfo
import util.IO

class BaseHtmlTest:
  val unresolvedLinkSelector = ".documentableBrief span[data-unresolved-link], .cover span[data-unresolved-link]"

  def projectName = "Test Project Name"
  def  projectVersion = "1.0.1-M1"

  case class ProjectContext(path: Path)

  def withGeneratedSite(base: Path)(op: ProjectContext ?=> Unit): Unit =
    withGeneratedDoc(Seq("site"), docsRoot = Some(base.toAbsolutePath.toString))(op)

  def withGeneratedDoc(
    pcks: Seq[String],
    docsRoot: Option[String] = None)(
    op: ProjectContext ?=> Unit,
    ): Unit =
      val dest = Files.createTempDirectory("test-doc")
      try
        val args = Scaladoc.Args(
            name = projectName,
            tastyFiles = pcks.flatMap(tastyFiles(_)),
            output = dest.toFile,
            docsRoot = docsRoot,
            projectVersion = Some(projectVersion)
          )
        Scaladoc.run(args)(using testContext)
        op(using ProjectContext(dest))

      finally IO.delete(dest.toFile)

  class DocumentContext(d: Document, path: Path):
    import collection.JavaConverters._

    def niceMsg(msg: String) = s"$msg in $path (body):\n ${d.html()}:\n"

    def assertTextsIn(selector: String, expected: String*) =
      assertFalse(niceMsg(s"Selector not found for '$selector'"), d.select(selector).isEmpty)
      val found = d.select(selector).eachText.asScala
      assertEquals(niceMsg(s"Context does not match for '$selector'"), expected.toList, found.toList)

    def assertAttr(selector: String, attr: String, expected: String*) =
      assertFalse(niceMsg(s"Selector '$selector' not found"), d.select(selector).isEmpty)
      val found = d.select(selector).eachAttr(attr).asScala
      assertEquals(niceMsg(s"Attribute $attr does not match for '$selector'"), expected.toList, found.toList)

    def assertNotExists(selector: String) =
      val msg = niceMsg(s"Selector '$selector' exisits in document")
      assertTrue(msg, d.select(selector).isEmpty)

  def withHtmlFile(pathStr: String)(op: DocumentContext => Unit)(using ProjectContext) =
    val path = summon[ProjectContext].path.resolve(pathStr)
    assertTrue(s"File at $path does not exisits!", Files.exists(path))
    val document = Jsoup.parse(IO.read(path))
    op(DocumentContext(document, path))
