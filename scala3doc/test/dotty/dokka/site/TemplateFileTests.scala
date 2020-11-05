package dotty.dokka.site

import com.vladsch.flexmark.html.HtmlRenderer
import com.vladsch.flexmark.parser.Parser
import org.junit.Assert.assertEquals
import org.junit.Test
import java.nio.file.Files

class TemplateFileTests:
  private def testTemplate(code: String, ext: String = "html")(op: TemplateFile => Unit): Unit =
    val tmpFile = Files.createTempFile("headerTests", s".${ext}").toFile()
    try
      Files.writeString(tmpFile.toPath, code)
      op(loadTemplateFile(tmpFile))
    finally tmpFile.delete()


  private def testTemplates(
                             props: Map[String, String],
                             template: List[(String, String)])(
                             op: RenderingContext => Unit
                           ) =
    def rec(cxt: RenderingContext, remaining: List[(String, String)]): Unit =
      if (remaining.isEmpty) op(cxt)
      else
        val (code, ext) = remaining.head
        testTemplate(code, ext) { template =>
          val newCtx = cxt.copy(layouts = cxt.layouts + (template.name() -> template))
          rec(newCtx, remaining.drop(1))
        }

    rec(RenderingContext(props), template)

  private def fullRender(template: TemplateFile, ctx: RenderingContext): String = template.resolveInner(ctx).code.trim()

  @Test
  def testParsingHeaders(): Unit =
    testTemplate(
      """---
        |title: myTitle
        |---
        |code""".stripMargin
    ) { t =>
      assertEquals(t.rawCode, "code")
      assertEquals(t.title(), "myTitle")
    }


  @Test
  def testLinks(): Unit =
    val base =
      """---
        |title: myTitle
        |name: base
        |---
        |Ala {{ content }}. {{p2}} with [link](link/target.md)!
        |""".stripMargin

    val content =
      """---
        |layout: base
        |name: content
        |---
        |ma kota w **{{ p1 }}** from [here](link/here.md)
        |""".stripMargin


    val expected = """<p>Ala ma kota w <strong>paski</strong> from <a href="link/here.md">here</a>. Hej with <a href="link/target.md">link</a>!</p>"""

    testTemplates(
      Map("p1" -> "paski", "p2" -> "Hej"),
      List(base -> "html", content -> "md")
    ) { it =>
      assertEquals(
        expected,
        fullRender(it.layouts("content"), it)
      )
    }

  @Test
  def layout(): Unit =
    val base =
      """---
        |title: myTitle
        |name: base
        |---
        |Ala {{ content }}. {{p2}}!
        |""".stripMargin

    val content =
      """---
        |layout: base
        |name: content
        |---
        |ma kota w **{{ p1 }}**
        |""".stripMargin


    val expected = """<p>Ala ma kota w <strong>paski</strong>. Hej!</p>""".stripMargin

    testTemplates(
      Map("p1" -> "paski", "p2" -> "Hej"),
      List(base -> "html", content -> "md")
    ) { it =>
      assertEquals(
        expected,
        fullRender(it.layouts("content"), it)
      )
    }

  @Test
  def nestedLayout_htmlMdHtml(): Unit =
    val toplevel =
      """---
        |name: toplevel
        |---
        |[div id="root"]{{ content }}[/div]
        |""".stripMargin

    val basePage =
      """---
        |layout: toplevel
        |name: basePage
        |---
        |# {{ pageName }}
        |
        |{{content}}
        |
        |## {{ pageName }} end
        |""".stripMargin

    val content =
      """---
        |layout: basePage
        |name: content
        |---
        |Hello {{ name }}!
        |""".stripMargin


    val expected =
      """[div id="root"][h1]Test page[/h1]
        |[p]Hello world!![/p]
        |[h2]Test page end[/h2]
        |[/div]""".stripMargin

    testTemplates(
      Map("pageName" -> "Test page", "name" -> "world!"),
      List(
        toplevel -> "html",
        basePage -> "md",
        content -> "md"
      )
    ) (it => fullRender(it.layouts("content"), it))

  @Test
  def nestedLayout_mdHtmlMd(): Unit =
    val toplevel =
      """---
        |name: toplevel
        |---
        |<h1>The Page</h1>
        |{{ content }}
        |""".stripMargin

    val basePage =
      """---
        |layout: toplevel
        |name: basePage
        |---
        |<h2>{{ pageName }}</h2>
        |
        |{{content}}
        |
        |<h3>{{ pageName }} end</h3>
        |""".stripMargin

    val content =
      """---
        |layout: basePage
        |name: content
        |---
        |Hello {{ name }}!
        |""".stripMargin


    val expected =
      """<h1>The Page</h1>
        |<h2>Test page</h2>
        |<p>Hello world!!</p>
        |<h3>Test page end</h3>""".stripMargin

    testTemplates(
      Map("pageName" -> "Test page", "name" -> "world!"),
      List(
        toplevel -> "html",
        basePage -> "html",
        content -> "md"
      )
    ) { ctx => assertEquals(expected, fullRender(ctx.layouts("content"), ctx)) }

  @Test
  def markdown(): Unit =
    testTemplate(
      """# Hello {{ msg }}!""",
      ext = "md"
    ) { t =>
      assertEquals("# Hello there!", t.resolveInner(RenderingContext(Map("msg" -> "there"))).code.trim())
    }

  @Test
  def mixedTemplates() : Unit =
    testTemplate(
      """# Hello {{ msg }}!""",
      ext = "md"
    ) { t =>
      assertEquals("# Hello there!", t.resolveInner(RenderingContext(Map("msg" -> "there"))).code.trim())
    }