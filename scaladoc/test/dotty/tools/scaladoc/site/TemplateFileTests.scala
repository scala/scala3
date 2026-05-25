package dotty.tools.scaladoc
package site

import com.vladsch.flexmark.html.HtmlRenderer
import com.vladsch.flexmark.parser.Parser
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test
import java.io.File
import java.nio.file.Files
import dotty.tools.scaladoc.assertMessagesAbout
import util.IO

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
        |<h2 class="h500"><a href="#test-page-end" class="anchor"></a>Test page end</h2>
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
        """<section id="hello-there2">
        |<h1 class="h500"><a href="#hello-there2" class="anchor"></a>Hello there2!</h1>
        |</section>""".stripMargin,
      t.resolveInner(RenderingContext(Map("msg" -> "there2"))).code.trim())
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

  @Test
  def snippetCompileWithErrorPointsToVisibleMarkdownLine(): Unit =
    val tmpFile = Files.createTempFile("snippet-position", ".md").toFile()
    try
      Files.write(
        tmpFile.toPath,
        """---
          |title: "Snippet position"
          |---
          |
          |```scala sc-hidden sc-name:preamble
          |import language.experimental.captureChecking
          |class File
          |```
          |
          |```scala sc-compile-with:preamble
          |object Console:oops
          |```
          |""".stripMargin.getBytes
      )

      val dctx = DocContext(
        testArgs().copy(snippetCompiler = List(s"${tmpFile.getAbsolutePath}=compile")),
        testContext
      )
      given StaticSiteContext = dctx.staticSiteContext.get

      loadTemplateFile(tmpFile).resolveInner(RenderingContext(Map.empty))
      summon[StaticSiteContext].reportSnippetMessages()

      val diagnostics = dctx.compilerContext.reportedDiagnostics
      assertEquals(diagnostics.errorMsgs.mkString("\n"), 1, diagnostics.errors.size)
      val error = diagnostics.errors.head
      assertTrue(error.message.contains("end of statement expected"))
      assertEquals(10, error.pos.line)
      assertEquals(14, error.pos.column)
    finally IO.delete(tmpFile)

  @Test
  def snippetErrorsAreBufferedAcrossTemplates(): Unit =
    val tmpRoot = Files.createTempDirectory("snippet-errors").toFile()
    val tmpDocs = File(tmpRoot, "_docs")
    val first = File(tmpDocs, "first.md")
    val second = File(tmpDocs, "second.md")
    try
      Files.createDirectories(tmpDocs.toPath)
      Files.write(
        first.toPath,
        """---
          |title: "First"
          |---
          |
          |```scala
          |val x = doesNotCompile
          |```
          |""".stripMargin.getBytes
      )
      Files.write(
        second.toPath,
        """---
          |title: "Second"
          |---
          |
          |```scala
          |val y = stillDoesNotCompile
          |```
          |""".stripMargin.getBytes
      )

      val dctx = DocContext(
        testArgs().copy(
          docsRoot = Some(tmpRoot.getAbsolutePath),
          snippetCompiler = List(s"${tmpDocs.getAbsolutePath}=compile")
        ),
        testContext
      )
      given StaticSiteContext = dctx.staticSiteContext.get

      loadTemplateFile(first).resolveInner(RenderingContext(Map.empty))
      loadTemplateFile(second).resolveInner(RenderingContext(Map.empty))

      assertEquals(0, dctx.compilerContext.reportedDiagnostics.errors.size)

      summon[StaticSiteContext].reportSnippetMessages()

      val diagnostics = dctx.compilerContext.reportedDiagnostics
      assertEquals(diagnostics.errorMsgs.mkString("\n"), 2, diagnostics.errors.size)
      assertMessagesAbout(diagnostics.errorMsgs)(
        "doesNotCompile",
        "stillDoesNotCompile"
      )
    finally IO.delete(tmpRoot)

  private def renderNamedSnippet(relativePath: String, noSnippetNamesFor: List[String] = Nil): String =
    val tmpRoot = Files.createTempDirectory("snippet-name-rendering").toFile()
    val tmpFile = File(tmpRoot, relativePath)
    try
      Files.createDirectories(tmpFile.getParentFile.toPath)
      Files.write(
        tmpFile.toPath,
        """---
          |title: "Snippet names"
          |---
          |
          |```scala sc-name:demo
          |val xs = List(1, 2, 3)
          |```
          |""".stripMargin.getBytes
      )

      val dctx = DocContext(
        testArgs().copy(
          docsRoot = Some(tmpRoot.getAbsolutePath),
          snippetCompiler = List(s"${tmpFile.getAbsolutePath}=compile"),
          noSnippetNamesFor = noSnippetNamesFor
        ),
        testContext
      )
      given StaticSiteContext = dctx.staticSiteContext.get

      loadTemplateFile(tmpFile).resolveInner(RenderingContext(Map.empty)).code
    finally IO.delete(tmpRoot)

  @Test
  def namedSnippetLabelsRenderByDefault(): Unit =
    val rendered = renderNamedSnippet("_docs/snippets.md")
    assertTrue(rendered.contains("""<div class="snippet-label">demo</div>"""))

  @Test
  def namedSnippetLabelsCanBeSuppressedForConfiguredPaths(): Unit =
    val rendered = renderNamedSnippet("_docs/snippets.md", noSnippetNamesFor = List("_docs"))
    assertTrue(!rendered.contains("""<div class="snippet-label">demo</div>"""))

  @Test
  def namedSnippetLabelsAreSuppressedInLanguageReference(): Unit =
    val rendered = renderNamedSnippet("_docs/reference/snippets.md", noSnippetNamesFor = List("_docs/reference"))
    assertTrue(!rendered.contains("""<div class="snippet-label">demo</div>"""))
