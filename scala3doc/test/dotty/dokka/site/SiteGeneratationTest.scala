package dotty.dokka
package site

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import org.junit.Test
import org.junit.Assert._
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import java.nio.charset.Charset
import dotty.dokka.test.BuildInfo

class SiteGeneratationTest:
  val projectName = "Test Project Name"
  val projectVersion = "1.0.1-M1"

  case class ProjectContext(path: Path)

  private def withGeneratedSite(base: Path)(op: ProjectContext ?=> Unit) =
    val dest = Files.createTempDirectory("test-doc")
    try
      val args = Scala3doc.Args(
          name = projectName,
          tastyFiles = tastyFiles("links"),
          output = dest.toFile,
          docsRoot = Some(base.toAbsolutePath.toString),
          projectVersion = Some(projectVersion),
        )
      Scala3doc.run(args)(using testContext)
      op(using ProjectContext(dest))

    finally IO.delete(dest.toFile)

  val testDocPath = Paths.get(BuildInfo.testDocumentationRoot)

  class DocumentContext(d: Document, path: Path):
    import collection.JavaConverters._

    def niceMsg(msg: String) = s"$msg in $path (body):\n ${d.html()}:\n"

    def assertTextsIn(selector: String, expected: String*) =
      assertFalse(niceMsg("Selector not found"), d.select(selector).isEmpty)
      val found = d.select(selector).eachText.asScala
      assertEquals(niceMsg(s"Context does not match for '$selector'"), expected.toList, found.toList)

    def assertAttr(selector: String, attr: String, expected: String*) =
      assertFalse(niceMsg("Selector not found"), d.select(selector).isEmpty)
      val found = d.select(selector).eachAttr(attr).asScala
      assertEquals(niceMsg(s"Attribute $attr does not match for '$selector'"), expected.toList, found.toList)

  def withHtmlFile(path: Path)(op: DocumentContext => Unit) = {
    assertTrue(s"File at $path does not exisits!", Files.exists(path))
    val document = Jsoup.parse(IO.read(path))
    op(DocumentContext(document, path))
  }

  def indexLinks(content: DocumentContext) =
    content.assertAttr("p a","href", "docs/index.html")

  def checkFile(
    path: String)(
    title: String,
    header: String,
    parents: Seq[String] = Nil,
    checks: DocumentContext => Unit = _ => ())(using ProjectContext) =
      withHtmlFile(summon[ProjectContext].path.resolve(path)){ content  =>
        content.assertTextsIn(".projectName", projectName)
        content.assertTextsIn(".projectVersion", projectVersion)
        content.assertTextsIn("h1", header)
        content.assertTextsIn("title", title)
        content.assertTextsIn(".breadcrumbs a", (parents :+ title):_*)
        checks(content)
      }

  def testDocPages()(using ProjectContext) =
    checkFile("docs/Adoc.html")(title = "Adoc", header = "Header in Adoc", parents = Seq(projectName))
    checkFile("docs/Adoc.html")(title = "Adoc", header = "Header in Adoc", parents = Seq(projectName))
    checkFile("docs/dir/index.html")(title = "A directory", header = "A directory", parents = Seq(projectName))
    checkFile("docs/dir/nested.html")(
      title = "Nested in a directory", header = "Nested in a directory", parents = Seq(projectName, "A directory"))

  def testDocIndexPage()(using ProjectContext) =
    checkFile("docs/index.html")(title = projectName, header = s"$projectName in header")

  def testMainIndexPage()(using ProjectContext) =
    checkFile("index.html")(title = "Basic test", header = "Header", parents = Seq(projectName), indexLinks)

  def testApiPages(
    mainTitle: String = "API",
    parents: Seq[String] = Seq(projectName))(using ProjectContext) =
      checkFile("api/index.html")(
        title = mainTitle,
        header = projectName,
        parents = parents
      )
      checkFile("api/tests/links.html")(
        title = "tests.links",
        header = "tests.links",
        parents = parents :+ mainTitle
      )
      checkFile("api/tests/links/LinksTest.html")(
        title = "LinksTest",
        header = "LinksTest",
        parents = parents ++ Seq(mainTitle, "tests.links")
      )


  @Test
  def basicTest() = withGeneratedSite(testDocPath.resolve("basic")){
    testDocPages()
    testDocIndexPage()
    testMainIndexPage()
    testApiPages()
  }

  @Test
  def noGlobalIndexTest() = withGeneratedSite(testDocPath.resolve("noGlobalIndex")){
    testDocPages()
    testDocIndexPage()
    testApiPages()
  }

  @Test
  def noIndexesTest() = withGeneratedSite(testDocPath.resolve("noIndexes")){
    testDocPages()
    testApiPages()
  }

  @Test
  def noExistingDocs() = withGeneratedSite(testDocPath.resolve("noExisting")){
    testApiPages(mainTitle = projectName, parents = Nil)
  }