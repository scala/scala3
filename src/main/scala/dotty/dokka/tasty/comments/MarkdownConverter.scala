package dotty.dokka.tasty.comments

import scala.jdk.CollectionConverters._
import kotlin.collections.builders.{ListBuilder => KtListBuilder, MapBuilder => KtMapBuilder}

object kt {
  def emptyList[T] = new KtListBuilder[T]().build()
  def emptyMap[A, B] = new KtMapBuilder[A, B]().build()
}

import org.jetbrains.dokka.model.{doc => dkkd}
import com.vladsch.flexmark.{ast => mda}
import com.vladsch.flexmark.util.{ast => mdu}
import com.vladsch.flexmark.ext.gfm.{tables => mdt}

object dkk {
    def text(str: String) =
      dkkd.Text(str, kt.emptyList, kt.emptyMap)
}

object MarkdownConverter {
  def convertChildren(n: mdu.Node): Seq[dkkd.DocTag] =
    n.getChildIterator.asScala.map(convertNode).toList

  def convertNode(n: mdu.Node): dkkd.DocTag = n match {
    case n: (mdu.Document | mda.Paragraph) =>
      dkkd.P(convertChildren(n).asJava, kt.emptyMap)

    case n: mda.Heading => n.getLevel match {
        case 1 => dkkd.H1(List(dkk.text(n.getText().toString)).asJava, kt.emptyMap)
        case 2 => dkkd.H2(List(dkk.text(n.getText().toString)).asJava, kt.emptyMap)
        case 3 => dkkd.H3(List(dkk.text(n.getText().toString)).asJava, kt.emptyMap)
        case 4 => dkkd.H4(List(dkk.text(n.getText().toString)).asJava, kt.emptyMap)
        case 5 => dkkd.H5(List(dkk.text(n.getText().toString)).asJava, kt.emptyMap)
        case 6 => dkkd.H6(List(dkk.text(n.getText().toString)).asJava, kt.emptyMap)
    }

    case n: mda.Text => dkk.text(n.getChars.toString)
    case n: mda.Emphasis =>
      // TODO doesn't actually show up in output, why?
      n.getOpeningMarker.toString match {
        case "*" => dkkd.B(convertChildren(n).asJava, kt.emptyMap)
        case "_" => dkkd.I(convertChildren(n).asJava, kt.emptyMap)
      }
    case n: mda.Code => dkkd.CodeInline(convertChildren(n).asJava, kt.emptyMap)
    case n: mda.IndentedCodeBlock =>
      dkkd.CodeBlock(List(dkk.text(n.getChars.toString)).asJava, kt.emptyMap)
    case n: mda.FencedCodeBlock =>
      // n.getInfo - where to stick this?
      dkkd.CodeBlock(convertChildren(n).asJava, kt.emptyMap)

    case n: mda.ListBlock =>
      val c = convertChildren(n).asJava
      n match {
        case _: mda.OrderedList => dkkd.Ol(c, kt.emptyMap)
        case _ => dkkd.Ul(c, kt.emptyMap)
      }
    case n: mda.ListItem =>
      dkkd.Li(convertChildren(n).asJava, kt.emptyMap)

    case n: mda.BlockQuote =>
      dkkd.BlockQuote(convertChildren(n).asJava, kt.emptyMap)

    case n: mdt.TableBlock =>
      // the structure is:
      // TableBlock {
      //   TableHeader {
      //     TableRow {
      //       TableCell { ... }
      //       TableCell { ... }
      //     }
      //   }
      //   TableSeparator { TableRow { ... } }
      //   TableBody { TableRow { ... } ... }
      // }
      val header =
        n.getFirstChild.getChildIterator.asScala.map { nn =>
          dkkd.Tr(
            nn.getChildIterator.asScala.map { nnn =>
              dkkd.Th(convertChildren(nnn).asJava, kt.emptyMap)
            }.toSeq.asJava,
            kt.emptyMap
          )
        }

      val body =
        n.getChildIterator.asScala.drop(2).next.getChildIterator.asScala.map { nn =>
          dkkd.Tr(
            nn.getChildIterator.asScala.map { nnn =>
              dkkd.Td(convertChildren(nnn).asJava, kt.emptyMap)
            }.toSeq.asJava,
            kt.emptyMap
          )
        }

      dkkd.Table(
        (header ++ body).toSeq.asJava,
        kt.emptyMap
      )

    case _: mda.SoftLineBreak => dkkd.Br.INSTANCE

    case _ =>
      println(s"!!! DEFAULTING @ ${n.getNodeName}")
      dkkd.P(
        List(
          dkkd.Span(
            List(dkk.text(s"!!! DEFAULTING @ ${n.getNodeName}")).asJava,
            Map("class" -> "lmao").asJava
          ),
          dkk.text(HtmlParsers.renderToText(n))
        ).asJava,
        kt.emptyMap
      )
  }

  object dbg {
    case class See(n: mdu.Node, c: Seq[See]) {
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

    def see(n: mdu.Node): See =
      See(n, n.getChildIterator.asScala.map(see).toList)

    def parseRaw(str: String) =
      MarkdownParser((), ()).stringToMarkup(str)

    def parse(str: String) =
      parseRaw( Preparser.preparse( Cleaner.clean(str) ).body )
  }
}
