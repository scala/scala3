package dotty.tools
package dottydoc

import vulpix.TestConfiguration

import org.junit.Test
import org.junit.Assert._

import dotc.core.Contexts.{ Context, ContextBase, FreshContext }
import dotc.core.Comments.{ ContextDoc, ContextDocstrings }
import dottydoc.core.ContextDottydoc

class MarkdownTests extends DottyDocTest with CheckFromSource {
  override implicit val ctx: FreshContext = {
    // TODO: check if can reuse parent instead of copy-paste
    val base = new ContextBase
    import base.settings._
    val ctx = base.initialCtx.fresh
    ctx.setSetting(ctx.settings.language, List("Scala2"))
    ctx.setSetting(ctx.settings.YcookComments, true)
    ctx.setSetting(ctx.settings.YnoInline, true)
    ctx.setSetting(ctx.settings.Ycheck, "all" :: Nil)
    // No wiki syntax!
    ctx.setSetting(ctx.settings.wikiSyntax, false)
    ctx.setProperty(ContextDoc, new ContextDottydoc)
    ctx.setSetting(
      ctx.settings.classpath,
      TestConfiguration.basicClasspath
    )
    base.initialize()(ctx)
    ctx
  }

  @Test def simpleMarkdown = {
    val source =
      """
      |package scala
      |
      |/** *Hello*, world! */
      |trait HelloWorld
      """.stripMargin

    checkSource(source) { (ctx, packages) =>
      val traitCmt =
        packages("scala")
        .children.find(_.path.mkString(".") == "scala.HelloWorld")
        .flatMap(_.comment.map(_.body))
        .get
        .trim

      assertEquals("<p><em>Hello</em>, world!</p>", traitCmt)
    }
  }

  @Test def outerLink = {
    val source =
      """
      |package scala
      |
      |/** [out](http://www.google.com) */
      |trait HelloWorld
      """.stripMargin

    checkSource(source) { (ctx, packages) =>
      val traitCmt =
        packages("scala")
        .children.find(_.path.mkString(".") == "scala.HelloWorld")
        .flatMap(_.comment.map(_.body))
        .get
        .trim

      assertEquals("""<p><a href="http://www.google.com">out</a></p>""", traitCmt)
    }
  }

  @Test def relativeLink = {
    val source =
      """
      |package scala
      |
      |/** [None](./None.html) */
      |trait HelloWorld
      |
      |trait None
      """.stripMargin

    checkSource(source) { (ctx, packages) =>
      val traitCmt =
        packages("scala")
        .children.find(_.path.mkString(".") == "scala.HelloWorld")
        .flatMap(_.comment.map(_.body))
        .get
        .trim

      assertEquals("""<p><a href="./None.html">None</a></p>""", traitCmt)
    }
  }

  @Test def absoluteLink = {
    val source =
      """
      |package scala
      |
      |/** [None](scala.None) */
      |trait HelloWorld
      |
      |trait None
      """.stripMargin

    checkSource(source) { (ctx, packages) =>
      val traitCmt =
        packages("scala")
        .children.find(_.path.mkString(".") == "scala.HelloWorld")
        .flatMap(_.comment.map(_.body))
        .get
        .trim

      assertEquals("""<p><a href="../scala/None.html">None</a></p>""", traitCmt)
    }
  }

  @Test def handleLists = {
    val source =
      """
      |package scala
      |
      |/** - Item1
      | *  - Item2
      | *  - Item3
      | */
      |trait HelloWorld
      |
      |trait None
      """.stripMargin

    checkSource(source) { (ctx, packages) =>
      val traitCmt =
        packages("scala")
        .children.find(_.path.mkString(".") == "scala.HelloWorld")
        .flatMap(_.comment.map(_.body))
        .get
        .trim

      assertEquals(
        """|<ul>
            |<li>Item1</li>
            |<li>Item2</li>
            |<li>Item3</li>
            |</ul>""".stripMargin, traitCmt)
    }
  }

  @Test def handleNestedLists = {
    val source =
      """
      |package scala
      |
      |/** - Item1
      | *    - Item1a
      | *    - Item1b
      | *  - Item2
      | *  - Item3
      | */
      |trait HelloWorld
      |
      |trait None
      """.stripMargin

    checkSource(source) { (ctx, packages) =>
      val traitCmt =
        packages("scala")
        .children.find(_.path.mkString(".") == "scala.HelloWorld")
        .flatMap(_.comment.map(_.body))
        .get
        .trim

      assertEquals(
        """|<ul>
            |<li>Item1
            |<ul>
            |<li>Item1a</li>
            |<li>Item1b</li>
            |</ul>
            |</li>
            |<li>Item2</li>
            |<li>Item3</li>
            |</ul>""".stripMargin, traitCmt)
    }
  }

  @Test def handleCodeBlock = {
    val source =
      """
      |package scala
      |
      |/** ```scala
      | *  val x = 1 + 5
      | *  ```
      | */
      |trait HelloWorld
      |
      |trait None
      """.stripMargin

    checkSource(source) { (ctx, packages) =>
      val traitCmt =
        packages("scala")
        .children.find(_.path.mkString(".") == "scala.HelloWorld")
        .flatMap(_.comment.map(_.body))
        .get
        .trim

      assertEquals(
        """|<pre><code class="scala">val x = 1 + 5
            |</code></pre>""".stripMargin, traitCmt)
    }
  }

  @Test def handleCodeBlockJavaDocstring = {
    // the following works, but not when the start of the codeblock is on the
    // first line
    val source =
      """
      |package scala
      |
      |/**
      | *  ```scala
      | *  val x = 1 + 5
      | *  ```
      | */
      |trait HelloWorld
      |
      |trait None
      """.stripMargin

    checkSource(source) { (ctx, packages) =>
      val traitCmt =
        packages("scala")
        .children.find(_.path.mkString(".") == "scala.HelloWorld")
        .flatMap(_.comment.map(_.body))
        .get
        .trim

      assertEquals(
        """|<pre><code class="scala">val x = 1 + 5
            |</code></pre>""".stripMargin, traitCmt)
    }
  }

  @Test def docstringSummary = {
    val source =
      """
      |package scala
      |
      |/** This
      | *  ====
      | *  is a short text [that](http://google.com) should not be more than a
      | *  `few` lines long. This text *should* be shortened somewhere that is
      | *  appropriate for the **ui**. Might be here, or there or somewhere
      | *  else.
      | */
      |trait HelloWorld
      """.stripMargin

    checkSource(source) { (ctx, packages) =>
      val traitCmt =
        packages("scala")
        .children.find(_.path.mkString(".") == "scala.HelloWorld")
        .flatMap(_.comment.map(_.short))
        .get
        .trim

      assert(
        traitCmt.endsWith("Might be here...\n</p>"),
        s"""|docstring summary should strip the following docstring so that it ends in "Might be here..."
            |
            |$traitCmt""".stripMargin
      )
    }
  }

  @Test def docstringSummaryWithImage = {
    val source =
      """
      |package scala
      |
      |/** This
      | *  ====
      | *  should quit before ![alt text](https://whatever.com/1.png "Img Text"),
      | *  I shouldn't be visible.
      | */
      |trait HelloWorld
      """.stripMargin

    checkSource(source) { (ctx, packages) =>
      val traitCmt =
        packages("scala")
        .children.find(_.path.mkString(".") == "scala.HelloWorld")
        .flatMap(_.comment.map(_.short))
        .get
        .trim

      assert(
        !traitCmt.contains("<img") &&
        !traitCmt.contains("I shouldn't be visible."),
        s"""|docstring summary shouldn't contain image, error in `MarkdownShortener.scala`
            |
            |$traitCmt""".stripMargin)
    }

  }
}
