package dotty.dokka.tasty.comments
package wiki

import scala.jdk.CollectionConverters._

import org.jetbrains.dokka.model.{doc => dkkd}

import dotty.dokka.tasty.SymOps

class Converter(val repr: Repr) extends BaseConverter {
  import Emitter._

  // makeshift support for not passing an owner
  // see same in MarkdownConverter
  val qctx: repr.qctx.type = if repr == null then null else repr.qctx
  val owner: qctx.reflect.Symbol = if repr == null then null.asInstanceOf[qctx.reflect.Symbol] else repr.sym

  object SymOps extends SymOps[qctx.type](qctx)
  import SymOps._

  def convertBody(body: Body): dkkd.DocTag = {
    dkkd.P(
      collect {
        body.blocks.foreach(emitBlock)
      }.asJava,
      kt.emptyMap,
    )
  }

  def emitBlock(block: Block)(using Emitter[dkkd.DocTag]): Unit =
    block match {
      case Title(text, level) =>
        val content = convertInline(text)
        emit(level match {
          case 1 => dkkd.H1(content.asJava, kt.emptyMap)
          case 2 => dkkd.H2(content.asJava, kt.emptyMap)
          case 3 => dkkd.H3(content.asJava, kt.emptyMap)
          case 4 => dkkd.H4(content.asJava, kt.emptyMap)
          case 5 => dkkd.H5(content.asJava, kt.emptyMap)
          case 6 => dkkd.H6(content.asJava, kt.emptyMap)
        })

      case Paragraph(text) =>
        emit(dkkd.P(
          convertInline(text).asJava,
          kt.emptyMap,
        ))
      case Code(data: String) => emit(dkkd.CodeBlock(List(dkk.text(data)).asJava, kt.emptyMap))
      case HorizontalRule() => emit(dkkd.HorizontalRule.INSTANCE)
      case DefinitionList(items) =>
        sys.error("not supported yet: definition list")

      case UnorderedList(items) =>
        emit(dkkd.Ul(
          convertListItems(items).asJava,
          kt.emptyMap,
        ))

      case OrderedList(items, style) =>
        // TODO use style
        emit(dkkd.Ol(
          convertListItems(items).asJava,
          kt.emptyMap,
        ))
    }

  def convertListItems(items: Seq[Block]): Seq[dkkd.DocTag] = {
    import scala.collection.mutable.ListBuffer
    val listBld = ListBuffer.empty[dkkd.DocTag]
    var elemBld = ListBuffer.empty[dkkd.DocTag]

    items.foreach { i =>
      val c = convertBlock(i)
      c match {
        case Seq(list: (dkkd.Ul | dkkd.Ol)) =>
          elemBld.append(list)
        case c =>
          if !elemBld.isEmpty then {
            listBld.append(dkkd.Li(elemBld.result.asJava, kt.emptyMap))
            elemBld = ListBuffer.empty
          }
          elemBld.appendAll(c)
      }
    }

    if elemBld.nonEmpty then
      listBld.append(dkkd.Li(elemBld.result.asJava, kt.emptyMap))

    listBld.result
  }

  def convertBlock(block: Block): Seq[dkkd.DocTag] =
    collect { emitBlock(block) }

  def emitInline(inl: Inline)(using Emitter[dkkd.DocTag]): Unit = inl match {
    case Chain(items: Seq[Inline]) => items.foreach(emitInline)
    case Summary(text) => emitInline(text)
    case Text(text) => emit(dkk.text(text))
    case Italic(text) => emit(dkkd.I(convertInline(text).asJava, kt.emptyMap))
    case Bold(text) => emit(dkkd.B(convertInline(text).asJava, kt.emptyMap))
    case Underline(text) => emit(dkkd.U(convertInline(text).asJava, kt.emptyMap))
    case Monospace(text) => emit(dkkd.CodeInline(convertInline(text).asJava, kt.emptyMap))
    case Link(target, body) =>
      def resolveBody(default: String) =
        if !body.isEmpty
        then convertInline(body).asJava
        else Seq(dkk.text(default)).asJava

      emit(target match {
        case SchemeUri() =>
          dkkd.A(resolveBody(default = target), Map("href" -> target).asJava)
        case _ =>
          resolveLinkQuery(target, Some(body).filter(!_.isEmpty))
      })

    case _: (Superscript | Subscript | RepresentationLink | HtmlTag) =>
      sys.error("not yet supported: Superscript | Subscript | RepresentationLink | HtmlTag")
  }

  def convertInline(inl: Inline): Seq[dkkd.DocTag] =
    collect { emitInline(inl) }

  def resolveLinkQuery(queryStr: String, bodyOpt: Option[Inline]): dkkd.DocTag = {
    def resolveBody(default: String) =
      bodyOpt match {
        case Some(body) =>
          convertInline(body).asJava
        case None =>
          Seq(dkk.text(default)).asJava
      }

    withParsedQuery(queryStr) { query =>
      MemberLookup.lookup(using qctx)(query, owner) match {
        case Some((sym, targetText)) =>
          dkkd.DocumentationLink(sym.dri, resolveBody(default = targetText), kt.emptyMap)
        case None =>
          dkkd.A(resolveBody(default = query.join), Map("href" -> "#").asJava)
      }
    }
  }
}
