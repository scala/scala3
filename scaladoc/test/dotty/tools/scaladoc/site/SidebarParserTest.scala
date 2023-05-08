package dotty.tools.scaladoc
package site

import org.junit.Test
import org.junit.Assert._

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

  private val sidebarErrorNoTitle =
      """index: index.md
      |subsection:
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

  private val msg = "Error parsing YAML configuration file: Title is not provided."

  private def schemaMessage: String =
    s"""Static site YAML configuration file should comply with the following description:
      |The root element of static site needs to be <subsection>
      |`title` and `directory` properties are ignored in root subsection.
      |
      |<subsection>:
      |  title: <string> # optional - Default value is file name. Title can be also set using front-matter.
      |  index: <string> # optional - If not provided, default empty index template is generated.
      |  directory: <string> # optional - By default, directory name is title name in kebab case.
      |  subsection: # optional - If not provided, pages are loaded from the index directory
      |    - <subsection> | <page>
      |  # either index or subsection needs to be present
      |<page>:
      |  title: <string> # optional - Default value is file name. Title can be also set using front-matter.
      |  page: <string>
      |  hidden: <boolean> # optional - Default value is false.
      |
      |For more information visit:
      |https://docs.scala-lang.org/scala3/guides/scaladoc/static-site.html
      |""".stripMargin

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

  @Test(expected = classOf[IllegalArgumentException])
  def loadSidebarError(): Unit =
    assertEquals(
    Sidebar.Category(
      None,
      None,
      List(),
      None
    ),
    Sidebar.load(sidebarErrorNoTitle)(using testContext)
    )
    throw new IllegalArgumentException(s"$msg\n$schemaMessage")