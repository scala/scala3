package dotty.tools.scaladoc
package site

import org.junit.Test
import org.junit.Assert._

// TODO add negaitve and more details tests
class SidebarParserTest:

  private val sidebar = """pages:
    - title: Blog
    - title: My title
      page: my-page1.md
    - page: my-page2.md
    - page: my-page3/subsection
    - title: Reference
      subsection:
        - page: my-page3.md
    - index: my-page4/index.md
      subsection:
        - page: my-page4/my-page4.md
    - title: My subsection
      index: my-page5/index.md
      subsection:
        - page: my-page5/my-page5.md
    - subsection:
        - page: my-page7/my-page7.md
    - index: my-page6/index.md
      subsection:
        - index: my-page6/my-page6/index.md
          subsection:
            - page: my-page6/my-page6/my-page6.md
  """

  @Test
  def loadSidebar(): Unit = assertEquals(
    Sidebar.Root(
      None,
        List(
        Sidebar.Page(Some("Blog"), ""),
        Sidebar.Page(Some("My title"), "my-page1.md"),
        Sidebar.Page(None, "my-page2.md"),
        Sidebar.Page(None, "my-page3/subsection"),
        Sidebar.Category(Some("Reference"), None, List(Sidebar.Page(None, "my-page3.md")), None),
        Sidebar.Category(None, Some("my-page4/index.md"),  List(Sidebar.Page(None, "my-page4/my-page4.md")), None),
        Sidebar.Category(Some("My subsection"), Some("my-page5/index.md"),  List(Sidebar.Page(None, "my-page5/my-page5.md")), None),
        Sidebar.Category(None, None,  List(Sidebar.Page(None, "my-page7/my-page7.md")), None),
        Sidebar.Category(None, Some("my-page6/index.md"),  List(Sidebar.Category(None, Some("my-page6/my-page6/index.md"),  List(Sidebar.Page(None, "my-page6/my-page6/my-page6.md")), None)), None),
      )
    ),
    Sidebar.load(sidebar)
  )
