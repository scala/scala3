package dotty.tools.pc.tests

import java.net.URI

import dotty.tools.dotc.semanticdb.{DocumentPrinter, TextDocument}
import dotty.tools.pc.base.BasePCSuite

import org.junit.Test

class PcSemanticdbSuite extends BasePCSuite:

  @Test def `simple` =
    check(
      """|package a
         |
         |object O {
         |  val a = 123
         |  val b = a + 1
         |}""".stripMargin,
      """|package a
         |
         |object O/*<-a::O.*/ {
         |  val a/*<-a::O.a.*/ = 123
         |  val b/*<-a::O.b.*/ = a/*->a::O.a.*/ +/*->scala::Int#`+`(+4).*/ 1
         |}
         |""".stripMargin
    )

  @Test def `worksheet` =
    check(
      """|import $ivy.`org.kohsuke:github-api:1.114`
         |
         |object O {
         |  val a = 123
         |  val b = a + 1
         |}""".stripMargin,
      // local0 comes most likely from the script object use to wrap ScriptSource
      """|import $ivy.`org.kohsuke:github-api:1.114`
         |
         |object O/*<-_empty_::O.*/ {
         |  val a/*<-_empty_::O.a.*/ = 123
         |  val b/*<-_empty_::O.b.*/ = a/*->_empty_::O.a.*/ +/*->scala::Int#`+`(+4).*/ 1
         |}
         |""".stripMargin,
      filename = "A.worksheet.sc"
    )

  // the synthetic `NamedTuple.apply` of a field selection has no occurrence, unlike an explicit `apply`
  @Test def `named-tuple` =
    check(
      """|package a
         |
         |object O {
         |  val x: (name: String, age: Int) = (name = "a", age = 1)
         |  val y = x.name
         |  val z = x.apply(0)
         |  x match {
         |    case (name = n, age = a) => n
         |  }
         |}""".stripMargin,
      """|package a
         |
         |object O/*<-a::O.*/ {
         |  val x/*<-a::O.x.*/: (name: String/*->scala::Predef.String#*/, age: Int/*->scala::Int#*/) = (name = "a", age = 1)
         |  val y/*<-a::O.y.*/ = x/*->a::O.x.*/.name
         |  val z/*<-a::O.z.*/ = x/*->a::O.x.*/.apply/*->scala::NamedTuple.apply(+1).*/(0)
         |  x/*->a::O.x.*/ match {
         |    case (name = n/*<-local0*/, age = a/*<-local1*/) => n/*->local0*/
         |  }
         |}
         |""".stripMargin
    )

  def check(
      original: String,
      expected: String,
      filename: String = "A.scala"
  ): Unit =
    val uri = new URI(s"file:///$filename")
    val doc = presentationCompiler.semanticdbTextDocument(uri, original)
    val obtained = DocumentPrinter.textDocumentPrettyPrint(doc.get())
    assertNoDiff(expected, obtained)
