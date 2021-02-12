package dotty.tools.scaladoc
package site

import org.junit.Test
import org.junit.Assert._

// TODO add negaitve and more details tests
class SidebarParserTest:

  private val sidebar = """sidebar:
    - title: Blog
      url: blog/index.html
    - title: Reference
      subsection:
        - title: Overview
          url: docs/reference/overview.html
        - title: New Types
          subsection:
            - title: Intersection types
              url: docs/reference/new-types/intersection-types.html
            - title: Union types
              url: docs/reference/new-types/union-types.html
 """

  @Test
  def loadSidebar(): Unit =
    assertEquals(2, Sidebar.load(sidebar).size)
