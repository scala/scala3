package dotty.tools.scaladoc
package site

import org.junit.Test
import java.nio.file.Files

class IndexPagesTest extends BaseHtmlTest:


  private val baseArgs = Scaladoc.Args(
    name = projectName,
    tastyFiles = Seq("site").flatMap(tastyFiles(_)),
    output = Files.createTempDirectory("test-doc").toFile,
    projectVersion = Some(projectVersion)
  )

  @Test
  def staticSiteAndApiSubdirectory = gridTest(baseArgs.copy(docsRoot = Some(testDocPath.resolve("noIndexes").toAbsolutePath.toString), apiSubdirectory = true))

  @Test
  def staticSiteAndNOApiSubdirectoryAndReadyToGoIndex = gridTest(baseArgs.copy(docsRoot = Some(testDocPath.resolve("basic").toAbsolutePath.toString), apiSubdirectory = false))

  @Test
  def staticSiteAndApiSubdirectoryAndReadyToGoIndex = gridTest(baseArgs.copy(docsRoot = Some(testDocPath.resolve("basic").toAbsolutePath.toString), apiSubdirectory = true))

  @Test
  def staticSiteAndNOApiSubdirectory = gridTest(baseArgs.copy(docsRoot = Some(testDocPath.resolve("noIndexes").toAbsolutePath.toString), apiSubdirectory = false))

  @Test
  def NOstaticSiteAndApSubdirectory = gridTest(baseArgs.copy(docsRoot = None, apiSubdirectory = true))

  @Test
  def NOstaticSiteAndNOApiSubdirectory = gridTest(baseArgs.copy(docsRoot = None, apiSubdirectory = false))

  private def gridTest(args: Scaladoc.Args) = withGeneratedDoc(Seq.empty, None, customArgs = Some(args)) {
    println(args.output)
    withHtmlFile("index.html") { content  =>
      content.fileExists
    }
  }
