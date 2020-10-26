package dotty.dokka.tasty.comments

import scala.jdk.CollectionConverters._
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import org.jetbrains.dokka.model.{doc => dkkd}

import org.junit.Test

class CommentParserTest {
  import CommentParserTest._

  @Test def testMdBlockCode(): Unit = {
    val mdp = MarkdownCommentParser(null)
    val str = """```scala
                |is.an("actual code block")
                |with.multiple("lines")
                |```""".stripMargin

    val res = mdp.markupToDokka(mdp.stringToMarkup(str))
    assertSame(res,
      dkk.p()(dkk.codeBlock()(
        dkk.text("""is.an("actual code block")
                   |with.multiple("lines")
                   |""".stripMargin)
      )),
    )
  }

  @Test def testMdIndentedCode(): Unit = {
    val mdp = MarkdownCommentParser(null)
    val str = """    is.an("actual code block")
                |    with.multiple("lines")""".stripMargin

    val res = mdp.markupToDokka(mdp.stringToMarkup(str))
    assertSame(res,
      dkk.p()(dkk.codeBlock()(
        dkk.text("""is.an("actual code block")
                   |with.multiple("lines")""".stripMargin)
      )))
  }

  @Test def testMdAutolinks(): Unit = {
    val mdp = MarkdownCommentParser(null)
    val link = "http://www.google.com"
    val str = s"This is an autolink: $link"
    val res = mdp.markupToDokka(mdp.stringToMarkup(str))

    assertSame(res,
      { import dkk._
        p(p(
          text("This is an autolink: "),
          a("href" -> link)(text(link)),
        ))
      })
  }

  @Test def testMdWrappedAutolinks(): Unit = {
    val mdp = MarkdownCommentParser(null)
    val link = "http://www.google.com"
    val str = s"This is an autolink: <$link>"
    val res = mdp.markupToDokka(mdp.stringToMarkup(str))

    assertSame(res,
      { import dkk._
        p(p(
          text("This is an autolink: "),
          a("href" -> link)(text(link)),
        ))
      })
  }

  @Test def testMdList(): Unit = {
    val mdp = MarkdownCommentParser(null)
    val str =
      """* a
        |  - a.a
        |  - a.b
        |  - a.c
        |* b
        |   1. b.1
        |   1. b.2
        |   1. b.3
        |     * b.3.a
        |     * b.3.b
        |     * b.3.c""".stripMargin

    val res = mdp.markupToDokka(mdp.stringToMarkup(str))
    assertSame(res,
      { import dkk._
        p(
          ul(
            li(
              p(text("a")),
              ul(
                li(p(text("a.a"))),
                li(p(text("a.b"))),
                li(p(text("a.c"))),
            )),
            li(
              p(text("b")),
              ol(
                li(p(text("b.1"))),
                li(p(text("b.2"))),
                li(
                  p(text("b.3")),
                  ul(
                    li(p(text("b.3.a"))),
                    li(p(text("b.3.b"))),
                    li(p(text("b.3.c"))),
        ))))))
      },
    )
  }

  @Test def testWikiList(): Unit = {
    val mdp = WikiCommentParser(null)
    val str =
      """ - a
        |   - a.a
        |   - a.b
        |   - a.c
        | - b
        |   1. b.1
        |   1. b.2
        |   1. b.3
        |      a. b.3.a
        |      a. b.3.b
        |      a. b.3.c""".stripMargin

    val res = mdp.markupToDokka(mdp.stringToMarkup(str))
    assertSame(res,
      { import dkk._
        p(
          ul(
            li(
              p(text("a")),
              ul(
                li(p(text("a.a"))),
                li(p(text("a.b"))),
                li(p(text("a.c"))),
            )),
            li(
              p(text("b")),
              ol(
                li(p(text("b.1"))),
                li(p(text("b.2"))),
                li(
                  p(text("b.3")),
                  ol(
                    li(p(text("b.3.a"))),
                    li(p(text("b.3.b"))),
                    li(p(text("b.3.c"))),
        ))))))
      },
    )
  }
}

object CommentParserTest {

  enum TagComparison {
    case OK(self: Class[_], children: List[TagComparison], params: List[ParamComparison])
    case Mismatch(expCls: Class[_], gotCls: Class[_], gotTag: dkkd.DocTag)
    case TextOK(text: String, children: List[TagComparison], params: List[ParamComparison])
    case TextMismatch(expStr: String, gotStr: String)
    case Missing(tag: dkkd.DocTag)
    case Extra(tag: dkkd.DocTag)
  }

  enum ParamComparison {
    case OK(name: String, value: String)
    case Mismatch(name: String, exp: String, got: String)
    case Missing(name: String, value: String)
    case Extra(name: String, value: String)
  }

  class TagComparisonBuilder(
    val parent: Option[TagComparisonBuilder]
  ) {
    private var failed: Boolean = false
    private var abortedWith: Option[TagComparison] = None
    private val childrenBld: ListBuffer[TagComparison] = ListBuffer.empty
    private val paramsBld: ListBuffer[ParamComparison] = ListBuffer.empty

    def emit(cmp: ParamComparison): Unit = {
      cmp match {
        case _: ParamComparison.OK => ;
        case _ =>
          fail()
      }
      paramsBld.append(cmp)
    }

    def emit(cmp: TagComparison): Unit = {
      cmp match {
        case _: (TagComparison.OK | TagComparison.TextOK) => ;
        case _ => fail()
      }
      childrenBld.append(cmp)
    }

    def child: TagComparisonBuilder =
      TagComparisonBuilder(parent = Some(this))

    def abort(res: TagComparison): Unit = {
      failed = true
      abortedWith = Some(res)
      parent.foreach(_.fail())
    }

    def fail(): Unit = {
      failed = true
      parent.foreach(_.fail())
    }

    def hasFailed = failed

    def result(exp: dkkd.DocTag) = abortedWith.getOrElse(exp match {
      case exp: dkkd.Text =>
        TagComparison.TextOK(exp.getBody, childrenBld.result, paramsBld.result)
      case exp =>
        TagComparison.OK(exp.getClass, childrenBld.result, paramsBld.result)
    })
  }

  def compareTags(exp: dkkd.DocTag, got: dkkd.DocTag): (TagComparison, Boolean) = {
    val bld = TagComparisonBuilder(parent = None)
    doCompareTags(bld)(exp, got)
    (bld.result(exp), bld.hasFailed)
  }

  def doCompareTags(bld: TagComparisonBuilder)(exp: dkkd.DocTag, got: dkkd.DocTag): Unit =
    (exp, got) match {
      case (_, _) if exp.getClass != got.getClass =>
        bld.abort(TagComparison.Mismatch(expCls = exp.getClass, gotCls = got.getClass, gotTag = got))
      case (exp: dkkd.Text, got: dkkd.Text) if exp.getBody != got.getBody =>
        bld.abort(TagComparison.TextMismatch(expStr = exp.getBody, gotStr = got.getBody))
      case _ =>
        val propmap = mutable.Map.empty[String, ParamComparison]
        got.getParams.asScala.foreach { (k, v) =>
          propmap(k) = ParamComparison.Extra(k, v)
        }
        exp.getParams.asScala.foreach { (k, v) =>
          propmap.get(k) match {
            case None =>
              propmap(k) = ParamComparison.Missing(k, v)
            case Some(ParamComparison.Extra(_, gotV)) =>
              if gotV == v then
                propmap(k) = ParamComparison.OK(k, v)
              else
                propmap(k) = ParamComparison.Mismatch(k, exp = v, got = gotV)
            case other =>
              sys.error(s"unexpected param comparison: $other")
          }
        }
        propmap.values.foreach(bld.emit)
        val expIter = exp.getChildren.asScala.iterator
        val gotIter = got.getChildren.asScala.iterator
        while expIter.hasNext || gotIter.hasNext do
          if !expIter.hasNext then
            bld.emit(TagComparison.Extra(gotIter.next))
          else if !gotIter.hasNext then
            bld.emit(TagComparison.Missing(expIter.next))
          else {
            val exp = expIter.next
            val got = gotIter.next
            val childBld = bld.child
            doCompareTags(childBld)(exp, got)
            bld.emit(childBld.result(exp))
          }
    }


  def doRender(bld: StringBuilder, indent: Int)(cmp: TagComparison): Unit = {
    def doIndent(ind: Int = indent): Unit =
      bld ++= " " * ind

    def doLn(ln: String, ind: Int = indent): Unit =
      doIndent(ind)
      bld ++= ln
      bld += '\n'

    def doText(text: String, ind: Int = indent): Unit =
      var firstLine = true
      text.linesIterator.foreach { ln =>
        if !firstLine then bld ++= "\n" else firstLine = false
        doIndent(ind)
        bld ++= ln
        bld
      }

    def renderTag(indent: Int)(tag: dkkd.DocTag): Unit = {
      tag match {
        case tag: dkkd.Text =>
          doLn("Text:", indent)
          doText(tag.getBody, indent + 2)
        case tag =>
          doIndent(indent)
          bld ++= tag.getClass.getSimpleName
          bld ++= "(\n"
          var firstChild = true
          tag.getChildren.asScala.foreach { c =>
            if !firstChild then bld += '\n' else firstChild = false
            renderTag(indent + 2)(c)
          }
          bld ++= "\n"
          doIndent(indent)
          bld ++= ")"
      }
    }

    cmp match {
      case TagComparison.TextOK(text, children, props) =>
        doLn("Text:")
        doText(text, indent + 2)
      case TagComparison.TextMismatch(expStr, gotStr) =>
        doLn("!!! MISMATCH: Text:")
        doLn("Expected:", indent + 2)
        doText(expStr, indent + 4)
        bld += '\n'
        doLn("Got:", indent + 2)
        doText(gotStr, indent + 4)
      case TagComparison.OK(cls, children, props) =>
        doIndent()
        bld ++= cls.getSimpleName
        bld ++= "(\n"
        var firstChild = true
        children.foreach { c =>
          if !firstChild then bld += '\n' else firstChild = false
          doRender(bld, indent + 2)(c)
        }
        bld ++= "\n"
        doIndent()
        bld ++= ")"
      case TagComparison.Mismatch(expCls, gotCls, gotTag) =>
        doLn(s"!!! MISMATCH: expected=${expCls.getSimpleName}, got=${gotCls.getSimpleName}, tag:")
        renderTag(indent + 2)(gotTag)
      case TagComparison.Extra(tag) =>
        doLn(s"!!! EXTRA:")
        renderTag(indent + 2)(tag)
      case TagComparison.Missing(tag) =>
        doLn(s"!!! MISSING:")
        renderTag(indent + 2)(tag)
    }
  }

  def assertSame(got: dkkd.DocTag, exp: dkkd.DocTag, explode: Boolean = false) = {
    val (comparison, failure) = compareTags(exp, got)
    if explode || failure then {
      val bld = new StringBuilder
      bld += '\n'
      doRender(bld, indent = 0)(comparison)
      bld += '\n'
      throw new java.lang.AssertionError(bld.toString)
    }
  }


  object DkkDbg {
    case class See(n: dkkd.DocTag, c: Seq[See]) {
      def show(sb: StringBuilder, indent: Int): Unit = {
        sb ++= " " * indent
        sb ++= n.toString
        sb ++= "\n"
        c.foreach { s => s.show(sb, indent + 2) }
      }

      override def toString = {
        val sb = new StringBuilder
        show(sb, 0)
        sb.toString
      }
    }

    def see(n: dkkd.DocTag): See =
      See(n, n.getChildren.asScala.map(see).toList)
  }
}
