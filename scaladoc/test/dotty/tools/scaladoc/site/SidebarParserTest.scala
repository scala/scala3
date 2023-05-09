package dotty.tools.scaladoc
package site

import org.junit.Test
import org.junit.Assert._
import dotty.tools.scaladoc.site.Sidebar
import dotty.tools.scaladoc.site.Sidebar.RawInput
import java.io.ByteArrayOutputStream
import java.io.PrintStream

// TODO add negaitve and more details tests
class SidebarParserTest:

  private val sidebar =
    """index: index.md
      |subsection:
      |  - title: My title
      |    page: my-page1.md
      |  - page: my-page2.md
      |  - page: my-page3/subsection
      |  - title: Reference
      |    subsection:
      |      - page: my-page3.md
      |        hidden: true
      |  - index: my-page4/index.md
      |    subsection:
      |      - page: my-page4/my-page4.md
      |  - title: My subsection
      |    index: my-page5/index.md
      |    subsection:
      |      - page: my-page5/my-page5.md
      |  - subsection:
      |      - page: my-page7/my-page7.md
      |  - index: my-page6/index.md
      |    subsection:
      |      - index: my-page6/my-page6/index.md
      |        subsection:
      |          - page: my-page6/my-page6/my-page6.md
      """.stripMargin

  private val sidebarErrorNoPage =
    """index: index.md
      |subsection:
      |  - title: My title
      |  - page: my-page2.md
      |  - page: my-page3/subsection
      |  - title: Reference
      |    subsection:
      |      - page: my-page3.md
      |        hidden: true
      |  - index: my-page4/index.md
      |    subsection:
      |      - page: my-page4/my-page4.md
      |  - title: My subsection
      |    index: my-page5/index.md
      |    subsection:
      |      - page: my-page5/my-page5.md
      |  - subsection:
      |      - page: my-page7/my-page7.md
      |  - index: my-page6/index.md
      |    subsection:
      |      - index: my-page6/my-page6/index.md
      |        subsection:
      |          - page: my-page6/my-page6/my-page6.md
      """.stripMargin

  private val msgNoTitle = "Error parsing YAML configuration file: Title is not provided."
  private val msgNoPage = "Error parsing YAML configuration file: Index or page path to at least one page is missing."

  private def schemaMessage: String = Sidebar.schemaMessage

  private val noPageExpectedError = s"$msgNoPage\n$schemaMessage\nPage my-page2.md does not exist.\nPage my-page3/subsection does not exist.\nPage my-page3.md does not exist.\nPage my-page4/my-page4.md does not exist.\nPage my-page5/my-page5.md does not exist.\nPage my-page7/my-page7.md does not exist.\nPage my-page6/my-page6/my-page6.md does not exist."

  @Test
  def loadSidebar(): Unit = assertEquals(
    Sidebar.Category(
      None,
      Some("index.md"),
      List(
        Sidebar.Page(Some("My title"), "my-page1.md", false),
        Sidebar.Page(None, "my-page2.md", false),
        Sidebar.Page(None, "my-page3/subsection", false),
        Sidebar.Category(Some("Reference"), None, List(Sidebar.Page(None, "my-page3.md", true)), None),
        Sidebar.Category(None, Some("my-page4/index.md"),  List(Sidebar.Page(None, "my-page4/my-page4.md", false)), None),
        Sidebar.Category(Some("My subsection"), Some("my-page5/index.md"),  List(Sidebar.Page(None, "my-page5/my-page5.md", false)), None),
        Sidebar.Category(None, None,  List(Sidebar.Page(None, "my-page7/my-page7.md", false)), None),
        Sidebar.Category(None, Some("my-page6/index.md"),  List(Sidebar.Category(None, Some("my-page6/my-page6/index.md"),  List(Sidebar.Page(None, "my-page6/my-page6/my-page6.md", false)), None)), None),
      ),
      None
    ),
    Sidebar.load(sidebar)(using testContext)
  )

  @Test
  def loadSidebarError(): Unit =
    val out = new ByteArrayOutputStream()
    Console.withErr(new PrintStream(out)) {
      Sidebar.load(sidebarErrorNoPage)(using testContext)
    }
    val error = out.toString().trim()
    assertEquals(noPageExpectedError, error)
