package dotty.dokka.tasty.comments

import scala.jdk.CollectionConverters._
import scala.tasty.Reflection

import org.jetbrains.dokka.model.{doc => dkkd}
import com.vladsch.flexmark.{ast => mda}
import com.vladsch.flexmark.util.{ast => mdu}
import com.vladsch.flexmark.ext.gfm.{tables => mdt}
import com.vladsch.flexmark.ext.{wikilink => mdw}

import dotty.dokka.tasty.SymOps

class MarkdownConverter(val r: Reflection)(owner: r.Symbol) {
  import Emitter._

  object SymOps extends SymOps[r.type](r)
  import SymOps._

  def convertDocument(doc: mdu.Document): dkkd.DocTag = {
    val res = collect {
      doc.getChildIterator.asScala.foreach(emitConvertedNode)
    }

    dkkd.P(res.asJava, kt.emptyMap)
  }

  def convertChildren(n: mdu.Node): Seq[dkkd.DocTag] =
    collect {
      n.getChildIterator.asScala.foreach(emitConvertedNode)
    }

  def emitConvertedNode(n: mdu.Node)(using Emitter[dkkd.DocTag]): Unit = n match {
    case n: mda.Paragraph =>
      emit(dkkd.P(convertChildren(n).asJava, kt.emptyMap))

    case n: mda.Heading => emit(n.getLevel match {
        case 1 => dkkd.H1(List(dkk.text(n.getText().toString)).asJava, kt.emptyMap)
        case 2 => dkkd.H2(List(dkk.text(n.getText().toString)).asJava, kt.emptyMap)
        case 3 => dkkd.H3(List(dkk.text(n.getText().toString)).asJava, kt.emptyMap)
        case 4 => dkkd.H4(List(dkk.text(n.getText().toString)).asJava, kt.emptyMap)
        case 5 => dkkd.H5(List(dkk.text(n.getText().toString)).asJava, kt.emptyMap)
        case 6 => dkkd.H6(List(dkk.text(n.getText().toString)).asJava, kt.emptyMap)
    })

    case n: mda.Text => emit(dkk.text(n.getChars.toString))
    // case n: mda.HtmlInline => dkkd.Br.INSTANCE
    case n: mda.Emphasis =>
      // TODO doesn't actually show up in output, why?
      emit(n.getOpeningMarker.toString match {
        case "*" => dkkd.B(convertChildren(n).asJava, kt.emptyMap)
        case "_" => dkkd.I(convertChildren(n).asJava, kt.emptyMap)
      })

    case n: mda.StrongEmphasis =>
      // TODO doesn't actually show up in output, why?
      // TODO distinguish between strong and regular emphasis?
      emit(n.getOpeningMarker.toString match {
        case "**" => dkkd.B(convertChildren(n).asJava, kt.emptyMap)
        case "__" => dkkd.I(convertChildren(n).asJava, kt.emptyMap)
      })

    case n: mda.Link =>
      val SchemeUri = """[a-z]+:.*""".r
      val userText: String = n.getText.toString
      val target: String = n.getUrl.toString
      def resolveText(default: String) =
        val resolved = if !userText.isEmpty then userText else default
        List(dkk.text(resolved)).asJava

      emit(target match {
        case SchemeUri() =>
          dkkd.A(resolveText(default = target), Map("href" -> target).asJava)
        case _ => MemberLookup.lookup(using r)(target, owner) match {
          case Some((sym, targetText)) =>
            dkkd.DocumentationLink(sym.dri, resolveText(default = targetText), kt.emptyMap)
          case None =>
            dkkd.A(resolveText(default = target), Map("href" -> "#").asJava)
        }
      })

    case n: mdw.WikiLink =>
      val (target, userText) =
        val chars: String = n.getChars.toString
        chars.substring(2, chars.length - 2).split(" ", /*max*/ 2) match {
          case Array(s) => (s, "")
          case Array(s1, s2) => (s1, s2)
        }

      def resolveText(default: String) =
        val resolved = if !userText.isEmpty then userText else default
        List(dkk.text(resolved)).asJava

      emit(MemberLookup.lookup(using r)(target, owner) match {
        case Some((sym, targetText)) =>
          dkkd.DocumentationLink(sym.dri, resolveText(default = targetText), kt.emptyMap)
        case None =>
          dkkd.A(resolveText(default = target), Map("href" -> "#").asJava)
      })

    case n: mda.Code => emit(dkkd.CodeInline(convertChildren(n).asJava, kt.emptyMap))
    case n: mda.IndentedCodeBlock =>
      emit(dkkd.CodeBlock(List(dkk.text(n.getChars.toString)).asJava, kt.emptyMap))
    case n: mda.FencedCodeBlock =>
      // n.getInfo - where to stick this?
      emit(dkkd.CodeBlock(convertChildren(n).asJava, kt.emptyMap))

    case n: mda.ListBlock =>
      val c = convertChildren(n).asJava
      emit(n match {
        case _: mda.OrderedList => dkkd.Ol(c, kt.emptyMap)
        case _ => dkkd.Ul(c, kt.emptyMap)
      })
    case n: mda.ListItem =>
      emit(dkkd.Li(convertChildren(n).asJava, kt.emptyMap))

    case n: mda.BlockQuote =>
      emit(dkkd.BlockQuote(convertChildren(n).asJava, kt.emptyMap))

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

      emit(dkkd.Table(
        (header ++ body).toSeq.asJava,
        kt.emptyMap
      ))

    case _: mda.SoftLineBreak => emit(dkkd.Br.INSTANCE)

    case _ =>
      println(s"!!! DEFAULTING @ ${n.getNodeName}")
      emit(dkkd.P(
        List(
          dkkd.Span(
            List(dkk.text(s"!!! DEFAULTING @ ${n.getNodeName}")).asJava,
            Map("class" -> "lmao").asJava
          ),
          dkk.text(HtmlParsers.renderToText(n))
        ).asJava,
        kt.emptyMap
      ))
  }

  def extractAndConvertSummary(doc: mdu.Document): Option[dkkd.DocTag] =
    doc.getChildIterator.asScala.collectFirst { case p: mda.Paragraph =>
      dkkd.P(convertChildren(p).asJava, kt.emptyMap)
    }
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
    MarkdownCommentParser(null, ()).stringToMarkup(str)

  def parse(str: String) =
    parseRaw( Preparser.preparse( Cleaner.clean(str) ).body )
}
