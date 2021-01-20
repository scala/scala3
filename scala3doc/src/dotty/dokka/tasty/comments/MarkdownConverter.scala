package dotty.dokka
package tasty.comments

import scala.jdk.CollectionConverters._

import org.jetbrains.dokka.model.{doc => dkkd}
import com.vladsch.flexmark.{ast => mda}
import com.vladsch.flexmark.util.{ast => mdu}
import com.vladsch.flexmark.ext.gfm.{tables => mdt}
import com.vladsch.flexmark.ext.{wikilink => mdw}

import dotty.dokka.tasty.SymOps

class MarkdownConverter(val repr: Repr) extends BaseConverter {
  import Emitter._

  // makeshift support for not passing an owner
  // see same in wiki.Converter
  val qctx: repr.qctx.type = if repr == null then null else repr.qctx
  val owner: qctx.reflect.Symbol = if repr == null then null.asInstanceOf[qctx.reflect.Symbol] else repr.sym

  object SymOps extends SymOps[qctx.type](qctx)
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
        case 1 => dkkd.H1(List(dkk.text(n.getText().toString)).asJava, JMap())
        // case -1 => dkkd.H1(List(dkk.text(n.getText().toString)).asJava, JMap()) // This does not compile but should!
        case 2 => dkkd.H2(List(dkk.text(n.getText().toString)).asJava, kt.emptyMap)
        case 3 => dkkd.H3(List(dkk.text(n.getText().toString)).asJava, kt.emptyMap)
        case 4 => dkkd.H4(List(dkk.text(n.getText().toString)).asJava, kt.emptyMap)
        case 5 => dkkd.H5(List(dkk.text(n.getText().toString)).asJava, kt.emptyMap)
        case 6 => dkkd.H6(List(dkk.text(n.getText().toString)).asJava, kt.emptyMap)
    })

    case n: mda.Text => emit(dkk.text(n.getChars.toString))
    case n: mda.TextBase =>
      // TextBase is a wrapper for other nodes that for unclear reasons
      // sometimes gets emitted (`AutoLink`s seem to be involved)
      n.getChildren.asScala.foreach(n => emitConvertedNode(n))

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

    case n: mda.AutoLink =>
      val url = n.getUrl.toString
      emit(dkkd.A(List(dkk.text(url)).asJava, Map("href" -> url).asJava))

    case n: mda.Link =>
      val body: String = n.getText.toString
      val target: String = n.getUrl.toString
      def resolveBody(default: String) =
        val resolved = if !body.isEmpty then body else default
        List(dkk.text(resolved)).asJava

      emit(dkkd.A(resolveBody(default = target), Map("href" -> target).asJava))

    case n: mdw.WikiLink =>
      val (target, body) =
        val chars = n.getChars.toString.substring(2, n.getChars.length - 2)
        MarkdownConverter.splitWikiLink(chars)

      def resolveBody(default: String) =
        val resolved = if !body.isEmpty then body else default
        List(dkk.text(resolved)).asJava

      emit(target match {
        case SchemeUri() =>
          dkkd.A(resolveBody(default = target), Map("href" -> target).asJava)
        case _ =>
          resolveLinkQuery(target, body)
      })

    case n: mda.Code =>
      emit(dkkd.CodeInline(convertChildren(n).asJava, kt.emptyMap))
    case n: mda.IndentedCodeBlock =>
      val bld = new StringBuilder
      n.getContentLines.asScala.foreach(bld append _)
      emit(dkkd.CodeBlock(List(dkk.text(bld.toString)).asJava, kt.emptyMap))
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

    case inline: mda.HtmlInline =>
      emit(dkkd.Html(List(dkk.text(inline.getSegments.mkString)).asJava, kt.emptyMap))

    case entity: mda.HtmlEntity =>
      emit(dkkd.Html(List(dkk.text(entity.getSegments.mkString)).asJava, kt.emptyMap))

    case block: mda.HtmlBlock =>
      emit(dkkd.Html(List(dkk.text(block.getContentChars.toString)).asJava, kt.emptyMap))

    // TODO (https://github.com/lampepfl/scala3doc/issues/205): for now just silent the warnigs
    case _: mda.LinkRef | _: com.vladsch.flexmark.ext.emoji.Emoji =>
      emit(dkk.text(MarkdownParser.renderToText(n)))

    case _ =>
      println(s"WARN: Encountered unrecognised Markdown node `${n.getNodeName}`, please open an issue.")
      emit(dkk.text(MarkdownParser.renderToText(n)))
  }

  def extractAndConvertSummary(doc: mdu.Document): Option[dkkd.DocTag] =
    doc.getChildIterator.asScala.collectFirst { case p: mda.Paragraph =>
      dkkd.P(convertChildren(p).asJava, kt.emptyMap)
    }

  def resolveLinkQuery(queryStr: String, body: String): dkkd.DocTag = {
    def resolveBody(default: String) =
      val resolved = if !body.isEmpty then body else default
      List(dkk.text(resolved)).asJava

    withParsedQuery(queryStr) { query =>
      MemberLookup.lookup(using qctx)(query, owner) match {
        case Some((sym, targetText)) =>
          dkkd.DocumentationLink(sym.dri.asDokka, resolveBody(default = targetText), kt.emptyMap)
        case None =>
          // println(s"WARN: Definition lookup for following query failed: $queryStr")
          dkkd.A(resolveBody(default = query.join), Map("title" -> s"Definition was not found: $queryStr", "href" -> "#").asJava)
      }
    }
  }
}

object MarkdownConverter {
  def splitWikiLink(chars: String): (String, String) =
    // split on a space which is not backslash escaped (regex uses "zero-width negative lookbehind")
    chars.split("(?<!(?<!\\\\)\\\\) ", /*max*/ 2) match {
      case Array(target) => (target, "")
      case Array(target, userText) => (target, userText)
    }
}
