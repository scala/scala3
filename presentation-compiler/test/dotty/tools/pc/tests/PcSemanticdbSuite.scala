package dotty.tools.pc.tests

import java.net.URI

import scala.language.unsafeNulls

import dotty.tools.dotc.semanticdb.{SymbolOccurrence, TextDocument}
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
         |object O/*a.O.*/ {
         |  val a/*a.O.a.*/ = 123
         |  val b/*a.O.b.*/ = a/*a.O.a.*/ +/*scala.Int#`+`(+4).*/ 1
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
         |object O/*_empty_.O.*/ {
         |  val a/*_empty_.O.a.*/ = 123
         |  val b/*_empty_.O.b.*/ = a/*_empty_.O.a.*/ +/*scala.Int#`+`(+4).*/ 1
         |}
         |""".stripMargin,
      filename = "A.worksheet.sc"
    )

  def check(
      original: String,
      expected: String,
      filename: String = "A.scala"
  ): Unit =
    val uri = new URI(s"file:///$filename")
    val doc = presentationCompiler.semanticdbTextDocument(uri, original)

    val document = TextDocument.parseFrom(doc.get())
    val withCode = document.withText(original)
    val obtained = printTextDocument(withCode)
    assertNoDiff(expected, obtained)

  import dotty.tools.dotc.semanticdb.Scala3.InfoOps.isPrimary
  import dotty.tools.dotc.semanticdb.Scala3.StringOps.isPackage
  import dotty.tools.pc.utils.TestExtensions.*

  def printTextDocument(doc: TextDocument): String =
    val symtab = doc.symbols.iterator.map(info => info.symbol -> info).toMap
    val sb = new StringBuilder
    val occurrences = doc.occurrences.sorted
    var offset = 0
    occurrences.foreach { occ =>
      val range = occ.range.get
      val (startPos, endPos) = range.getOffsets(doc.text)
      sb.append(doc.text.substring(offset, endPos))
      val isPrimaryConstructor =
        symtab.get(occ.symbol).exists(_.isPrimary)
      if !occ.symbol.isPackage && !isPrimaryConstructor then
        printSymbol(sb, occ.symbol)
      offset = endPos
    }
    sb.append(doc.text.substring(offset))
    sb.toString()

  def printSymbol(sb: StringBuilder, symbol: String): Unit =
    sb.append("/*")
      // replace package / with dot . to not upset GitHub syntax highlighting.
      .append(symbol.replace('/', '.'))
      .append("*/")

  implicit val occurrenceOrdering: Ordering[SymbolOccurrence] =
    new Ordering[SymbolOccurrence]:
      override def compare(x: SymbolOccurrence, y: SymbolOccurrence): Int =
        if x.range.isEmpty then 0
        else if y.range.isEmpty then 0
        else
          val a = x.range.get
          val b = y.range.get
          val byLine = Integer.compare(
            a.startLine,
            b.startLine
          )
          if byLine != 0 then
            byLine
          else
            val byEnd = Integer.compare(
              a.endCharacter,
              b.endCharacter
            )
            if byEnd != 0 then
              byEnd
            else
              val byCharacter = Integer.compare(
                a.startCharacter,
                b.startCharacter
              )
              byCharacter
