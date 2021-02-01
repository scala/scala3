package dotty.tools.scaladoc.renderers

import org.junit.{Test, Rule}
import org.junit.Assert.{assertSame, assertTrue, assertEquals}
import dotty.tools.scaladoc.util.HTML._

class HtmlTagsTest {

  @Test
  def simpleDiv = {
    val actual = div().toString
    val expect = "<div></div>"
    assertEquals(expect, actual)
  }

  @Test
  def divWithStyles = {
    val actual = div(style := "some: style;")().toString
    val expect = """<div style="some: style;"></div>"""
    assertEquals(expect, actual)
  }

  @Test
  def divWithChildren = {
    val actual = div(h1(), span()).toString
    val expect = """<div><h1></h1><span></span></div>"""
    assertEquals(expect, actual)
  }

  @Test
  def divWithTextInside = {
    val actual = div(h1(), span("Some text"), "Some more of the text").toString
    val expect = """<div><h1></h1><span>Some text</span>Some more of the text</div>"""
    assertEquals(expect, actual)
  }

  @Test
  def escapeAmpersand = {
    val actual = div("Some & text").toString
    val expect = """<div>Some &amp; text</div>"""
    assertEquals(expect, actual)
  }

  @Test
  def escapeLessThan = {
    val actual = div("Some < text").toString
    val expect = """<div>Some &lt; text</div>"""
    assertEquals(expect, actual)
  }

  @Test
  def escapeGreaterThan = {
    val actual = div("Some > text").toString
    val expect = """<div>Some &gt; text</div>"""
    assertEquals(expect, actual)
  }

  @Test
  def escapeQuotationMark = {
    val actual = div("Some \" text").toString
    val expect = """<div>Some &quot; text</div>"""
    assertEquals(expect, actual)
  }

  @Test
  def escapeApostrophe = {
    val actual = div("Some ' text").toString
    val expect = """<div>Some &apos; text</div>"""
    assertEquals(expect, actual)
  }

  @Test
  def nestedTagsWithAttributes = {
    val actual = html(
      head(
        script(src:="..."),
        script(raw("alert('Hello World')"))
      ),
      body(
        div(
          h1(id:="title")("This is a title"),
          p("This is a big paragraph of text")
        )
      )
    ).toString
    val expect = """<html><head><script src="..."></script><script>alert('Hello World')</script></head><body><div><h1 id="title">This is a title</h1><p>This is a big paragraph of text</p></div></body></html>"""
    assertEquals(expect, actual)
  }

  @Test
  def anotherNestedTagsWithAttributes = {
    val actual = html(
      head(
        script("some script")
      ),
      body(
        h1(style:="background-color: blue; color: red;")("This is my title"),
        div(style:="background-color: blue; color: red;")(
        p(cls :="contentpara first")(
          "This is my first paragraph"
        ),
        a(style:="opacity: 0.9;")(
          p(cls := "contentpara")("Goooogle")
        )
        )
      )
    ).toString
    val expect = """<html><head><script>some script</script></head><body><h1 style="background-color: blue; color: red;">This is my title</h1><div style="background-color: blue; color: red;"><p class="contentpara first">This is my first paragraph</p><a style="opacity: 0.9;"><p class="contentpara">Goooogle</p></a></div></body></html>"""
    assertEquals(expect, actual)
  }

  @Test
  def appliedTagAndSeqAppliedTag = {
    val actual = div(h1("AppliedTag"), Seq(h1("SeqAppliedTag"))).toString
    val expect = """<div><h1>AppliedTag</h1><h1>SeqAppliedTag</h1></div>"""
    assertEquals(expect, actual)
  }

  @Test
  def stringAndSeqString = {
    val actual = div("String", Seq("SeqString")).toString
    val expect = """<div>StringSeqString</div>"""
    assertEquals(expect, actual)
  }

  @Test
  def mixingAllTagArgs = {
    val actual = div("String", Seq("SeqString"), h1("AppliedTag"), Seq(h1("SeqAppliedTag")), Seq("SeqString"), h1("AppliedTag")).toString
    val expect = """<div>StringSeqString<h1>AppliedTag</h1><h1>SeqAppliedTag</h1>SeqString<h1>AppliedTag</h1></div>"""
    assertEquals(expect, actual)
  }

  @Test
  def appliedAttrAndSeqAppliedAttr = {
    val actual = div(cls := "someClass", Seq(style := "some: style;")).toString
    val expect = """<div class="someClass" style="some: style;"></div>"""
    assertEquals(expect, actual)
  }

  @Test
  def seqAppliedAttrAndAppliedAttr = {
    val actual = div(Seq(cls := "someClass"), style := "some: style;").toString
    val expect = """<div class="someClass" style="some: style;"></div>"""
    assertEquals(expect, actual)
  }

  @Test
  def seqAppliedAttrAndSeqAppliedAttr = {
    val actual = div(Seq(cls := "someClass"), Seq(style := "some: style;")).toString
    val expect = """<div class="someClass" style="some: style;"></div>"""
    assertEquals(expect, actual)
  }

  @Test
  def mixingAllTagArgsAndAllAttrArgs = {
    val actual = div(Seq(cls := "someClass"), id := "myId", Seq(style := "some: style;"))("String", Seq("SeqString"), h1("AppliedTag"), Seq(h1("SeqAppliedTag")), Seq("SeqString"), h1("AppliedTag")).toString
    val expect = """<div class="someClass" id="myId" style="some: style;">StringSeqString<h1>AppliedTag</h1><h1>SeqAppliedTag</h1>SeqString<h1>AppliedTag</h1></div>"""
    assertEquals(expect, actual)
  }

  @Test
  def nilTagArg = {
    val nil: TagArg = Nil
    val actual = div(nil).toString
    val expect = """<div></div>"""
    assertEquals(expect, actual)
  }

  @Test
  def nilAttrArg = {
    val nil: AttrArg = Nil
    val actual = div(nil).toString
    val expect = """<div></div>"""
    assertEquals(expect, actual)
  }

  @Test
  def nilAmongTags = {
    val actual = div("name", Nil, div("ala")).toString
    val expect = """<div>name<div>ala</div></div>"""
    assertEquals(expect, actual)
  }

  @Test
  def nilAmongArgs = {
    val actual = div(cls := "someClass", Nil, style := "some: style;").toString
    val expect = """<div class="someClass" style="some: style;"></div>"""
    assertEquals(expect, actual)
  }

  @Test
  def nilAmongArgsAndTags = {
    val actual = div(cls := "someClass", Nil, style := "some: style;")("name", Nil, div("ala")).toString
    val expect = """<div class="someClass" style="some: style;">name<div>ala</div></div>"""
    assertEquals(expect, actual)
  }
}
