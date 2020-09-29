package dotty.dokka.tasty.comments
package wiki

import scala.jdk.CollectionConverters._
import scala.tasty.Reflection

import org.jetbrains.dokka.model.{doc => dkkd}

import dotty.dokka.tasty.SymOps

class Converter(val repr: Repr) {
  import Emitter._

  // makeshift support for not passing an owner
  // see same in MarkdownConverter
  val r: repr.r.type = if repr == null then null else repr.r
  val owner: r.Symbol = if repr == null then null.asInstanceOf[r.Symbol] else repr.sym

  object SymOps extends SymOps[r.type](r)
  import SymOps._

  def convertBody(body: Body): dkkd.DocTag = {
    dkkd.P(
      collect {
        body.blocks.foreach(emitBlock(_))
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
    case Link(target, userText) =>
      val SchemeUri = """[a-z]+:.*""".r
      def resolveText(default: String) =
        if !userText.isEmpty
        then convertInline(userText).asJava
        else Seq(dkk.text(default)).asJava

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

    case _: (Superscript | Subscript | RepresentationLink | HtmlTag) =>
      sys.error("not yet supported: Superscript | Subscript | RepresentationLink | HtmlTag")
  }

  def convertInline(inl: Inline): Seq[dkkd.DocTag] =
    collect { emitInline(inl) }
}
