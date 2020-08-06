package dotty.dokka.tasty.comments
package wiki

import scala.jdk.CollectionConverters._
import scala.tasty.Reflection

import org.jetbrains.dokka.model.{doc => dkkd}

import dotty.dokka.tasty.SymOps

class Converter(val r: Reflection)(owner: r.Symbol) {
  import Emitter._

  object SymOps extends SymOps[r.type](r)
  import SymOps._

  def convertBody(body: Body): dkkd.DocTag = {
    dkkd.P(
      collect {
        body.blocks.foreach(emitBlock(_, isTopLevel = true))
      }.asJava,
      kt.emptyMap,
    )
  }

  def emitBlock(block: Block, isTopLevel: Boolean = false)(using Emitter[dkkd.DocTag]): Unit =
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
        if isTopLevel
        && !lastEmittedItem.exists(_.isInstanceOf[dkkd.H1 | dkkd.H2 | dkkd.H3 | dkkd.H4 | dkkd.H5 | dkkd.H6])
        then
          emit(dkkd.Br.INSTANCE)

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
          items.map { i =>
            dkkd.Li(convertBlock(i).asJava, kt.emptyMap)
          }.asJava,
          kt.emptyMap,
        ))

      case OrderedList(items, style) =>
        // TODO use style
        emit(dkkd.Ol(
          items.map { i =>
            dkkd.Li(convertBlock(i).asJava, kt.emptyMap)
          }.asJava,
          kt.emptyMap,
        ))
    }

  def convertBlock(block: Block, isTopLevel: Boolean = false): Seq[dkkd.DocTag] =
    collect { emitBlock(block, isTopLevel) }

  def emitInline(inl: Inline)(using Emitter[dkkd.DocTag]): Unit = inl match {
    case Chain(items: Seq[Inline]) => items.foreach(emitInline)
    case Summary(text) => emitInline(text)
    case Text(text) => emit(dkk.text(text))
    case Italic(text) => emit(dkkd.I(convertInline(text).asJava, kt.emptyMap))
    case Bold(text) => emit(dkkd.B(convertInline(text).asJava, kt.emptyMap))
    case Underline(text) => emit(dkkd.U(convertInline(text).asJava, kt.emptyMap))
    case Monospace(text) => emit(dkkd.CodeInline(convertInline(text).asJava, kt.emptyMap))
    case Link(target, text) =>
      val SchemeUri = """[a-z]+:.*""".r

      emit(target match {
        case SchemeUri() => dkkd.A(convertInline(text).asJava, Map("href" -> target).asJava)
        case _ => MemberLookup.lookup(using r)(target, owner) match {
          case Some(sym) =>
            println(s"dri of `${sym.show}` = ${sym.dri}")
            dkkd.DocumentationLink(sym.dri, convertInline(text).asJava, kt.emptyMap)
          case None => dkkd.A(convertInline(text).asJava, Map("href" -> "#").asJava)
        }
      })

    case _: (Superscript | Subscript | RepresentationLink | HtmlTag) =>
      sys.error("not yet supported: Superscript | Subscript | RepresentationLink | HtmlTag")
  }

  def convertInline(inl: Inline): Seq[dkkd.DocTag] =
    collect { emitInline(inl) }
}
