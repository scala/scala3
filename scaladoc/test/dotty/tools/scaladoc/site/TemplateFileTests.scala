package dotty.tools.scaladoc
package site

import com.vladsch.flexmark.html.HtmlRenderer
import com.vladsch.flexmark.parser.Parser
import org.junit.Assert.assertEquals
import org.junit.Test
import java.nio.file.Files

class TemplateFileTests:
  given staticSiteContext: StaticSiteContext = testDocContext().staticSiteContext.get
  private def testTemplate(code: String, ext: String = "html")(op: TemplateFile => Unit): Unit =
    val tmpFile = Files.createTempFile("headerTests", s".${ext}").toFile()
    try
      Files.write(tmpFile.toPath, code.getBytes)
      op(loadTemplateFile(tmpFile))
    finally tmpFile.delete()


  private def testContent(
                            expected: String,
                            props: Map[String, String],
                            template: List[(String, String)]
                          ) =
    def rec(ctx: RenderingContext, remaining: List[(String, String)]): Unit =
      if remaining.isEmpty then
        assertEquals(expected.trim(), ctx.layouts("content").resolveInner(ctx).code.trim())
      else
        val (code, ext) = remaining.head
        testTemplate(code, ext) { template =>
          val newCtx = ctx.copy(layouts = ctx.layouts + (template.name -> template))
          rec(newCtx, remaining.drop(1))
        }

    rec(RenderingContext(props), template)

  @Test
  def testParsingHeaders(): Unit =
    testTemplate(
      """---
        |title: myTitle
        |---
        |code""".stripMargin
    ) { t =>
      assertEquals(t.rawCode, "code")
      assertEquals(t.title.name, "myTitle")
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


    val expected =
    """<p>Ala ma kota w <strong>paski</strong> from <a href="link/here.md">here</a>. Hej with <a href="link/target.md">link</a>!</p>""".stripMargin

    testContent(
      expected,
      Map("p1" -> "paski", "p2" -> "Hej"),
      List(base -> "md", content -> "md")
    )

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


    val expected =
      """Ala <p>ma kota w <strong>paski</strong></p>
        |. Hej!""".stripMargin

    testContent(
      expected,
      Map("p1" -> "paski", "p2" -> "Hej"),
      List(base -> "html", content -> "md")
    )

  @Test
  def nestedLayout_htmlMdHtml(): Unit =
    val toplevel =
      """---
        |name: toplevel
        |---
        |<div id="root">{{ content }}</div>
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
      """<div id="root"><section id="test-page">
        |<h1 class="h500"><a href="#test-page" class="anchor"></a>Test page</h1>
        |<p>Hello world!!</p>
        |</section><section id="test-page-end">
        |<h2 class="h300"><a href="#test-page-end" class="anchor"></a>Test page end</h2>
        |</section>
        |</div>""".stripMargin

    testContent(
      expected,
      Map("pageName" -> "Test page", "name" -> "world!"),
      List(
        toplevel -> "html",
        basePage -> "md",
        content -> "md"
      )
    )

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
        |
        |<p>Hello world!!</p>
        |
        |
        |<h3>Test page end</h3>""".stripMargin

    testContent(
      expected,
      Map("pageName" -> "Test page", "name" -> "world!"),
      List(
        toplevel -> "html",
        basePage -> "html",
        content -> "md"
      )
    )
  @Test
  def markdown(): Unit =
    testTemplate(
      """# Hello {{ msg }}!""",
      ext = "md"
    ) { t =>
      assertEquals(
        """<section id="hello-there">
        |<h1 class="h500"><a href="#hello-there" class="anchor"></a>Hello there!</h1>
        |</section>""".stripMargin,
      t.resolveInner(RenderingContext(Map("msg" -> "there"))).code.trim())
    }

  @Test
  def mixedTemplates() : Unit =
    testTemplate(
      """# Hello {{ msg }}!""",
      ext = "md"
    ) { t =>
      assertEquals(
        """<section id="hello-there">
        |<h1 class="h500"><a href="#hello-there" class="anchor"></a>Hello there!</h1>
        |</section>""".stripMargin,
      t.resolveInner(RenderingContext(Map("msg" -> "there"))).code.trim())
    }

  @Test
  def htmlOnly(): Unit =
    val html =
    """<div>Ala</ala>
      |
      |<span>Ula</span>
      |""".stripMargin

    val base =
      """---
        |title: myTitle
        |name: base
        |---
        |{{ content }}
        |""".stripMargin

    val content =
      s"""---
         |layout: base
         |name: content
         |---
         |$html
         |""".stripMargin


    testContent(
      html,
      Map(),
      List(base -> "html", content -> "html"))
