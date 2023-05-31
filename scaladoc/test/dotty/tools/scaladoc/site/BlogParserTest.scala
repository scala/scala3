package dotty.tools.scaladoc
package site

import org.junit.Test
import org.junit.Assert._

class BlogParserTest:

  private val blogConfig =
    """input: blog
      |output: blog
      |hidden: false
      |""".stripMargin

  @Test
  def loadBlog(): Unit = assertEquals(
    BlogConfig("blog", "blog", false),
    BlogParser.readYml(blogConfig)
  )