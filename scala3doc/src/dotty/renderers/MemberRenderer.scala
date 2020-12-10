package dotty.dokka

import dotty.dokka.model.api._
import scala.collection.immutable.SortedMap
import dotty.dokka.HTML._
import org.jetbrains.dokka.base.transformers.pages.comments.DocTagToContentConverter
import org.jetbrains.dokka.pages.{DCI, ContentKind, ContentNode}
import org.jetbrains.dokka.model.properties.PropertyContainer
import collection.JavaConverters._

class MemberRenderer(signatureRenderer: SignatureRenderer, buildNode: ContentNode => String)(using DocContext):

  private val converter = new DocTagToContentConverter()

  def renderDocPart(d: DocPart): AppliedTag =
    val sb  = StringBuilder()
    converter.buildContent(
      d,
      DCI(JSet(), ContentKind.Comment),
      JSet(),
      JSet(),
      PropertyContainer.Companion.empty()
    ).forEach(c => sb.append(buildNode(c)))
    raw(sb)

  def fullSignature(m: Member): AppliedTag = raw("signature: TODO!")

  def doc(m: Member): Seq[AppliedTag] =  m.docs.fold(Nil)(d => Seq(renderDocPart(d.body)))

  def tableRow(name: String, content: AppliedTag) = Seq(dt(name), dd(content))


  def docAttributes(m: Member): Seq[AppliedTag] =

    def nested(name: String, on: SortedMap[String, DocPart]): Seq[AppliedTag] = if on.isEmpty then Nil else
      tableRow(name, dl(cls := "attributes")(
        on.map { case (name, value) => tableRow(name, renderDocPart(value))}.toList:_*
      ))

    def list(name: String, on: List[DocPart]): Seq[AppliedTag] = if on.isEmpty then Nil else
      tableRow(name, div(on.map(e => div(renderDocPart(e)))))

    def opt(name: String, on: Option[DocPart]): Seq[AppliedTag] = if on.isEmpty then Nil else
      tableRow(name, renderDocPart(on.get))

    m.docs.fold(Nil)(d =>
      nested("Type Params", d.typeParams) ++
      nested("Value Params", d.valueParams) ++
      opt("Returns", d.result) ++
      nested("Throws", d.throws.transform((_, v) => v._1)) ++
      opt("Constructor", d.constructor) ++
      list("Authors", d.authors) ++
      list("See also", d.see) ++
      opt("Version", d.version) ++
      opt("Since", d.since) ++
      list("Todo", d.todo) ++
      list("Note", d.note) ++
      list("Example", d.example)
    )

  def companion(m: Member): Seq[AppliedTag] = m.companion.fold(Nil){dri =>
    val kindName = if m.kind == Kind.Object then "class" else "object"
    tableRow("Companion", signatureRenderer.renderLink(kindName, dri))
  }

  def source(m: Member): Seq[AppliedTag] =
    summon[DocContext].sourceLinks.pathTo(m).fold(Nil){ link =>
      tableRow("Source", a(href := link)("(source)"))
    }

  def deprecation(m: Member): Seq[AppliedTag] = m.deprecated.fold(Nil){ a =>
    def stripQuotes(s: String) = s.stripPrefix("\"").stripSuffix("\"")
    def parameter(p: Annotation.AnnotationParameter): TagArg = p match
      case Annotation.PrimitiveParameter(_, value) => stripQuotes(value)
      case Annotation.LinkParameter(_, dri, text) =>
        signatureRenderer.renderLink(stripQuotes(text), dri)
      case Annotation.UnresolvedParameter(_, value) => stripQuotes(value)

    val (named, unnamed) = a.params.partition(_.name.nonEmpty)
    val message = named.find(_.name.get == "message").orElse(unnamed.headOption)
    val since = named.find(_.name.get == "since").orElse(unnamed.drop(1).headOption)

    val content =
      since.fold(Nil)(since =>
        Seq(code("[Since version ", parameter(since), "] ")) ++
          message.fold(Nil)(m => Seq(parameter(m))) ++
          m.docs.fold(Nil)(_.deprecated.toSeq.map(renderDocPart))
      )
    Seq(dt("Deprecated"), dd(content:_*))
  }

  def memberInfo(m: Member): Seq[AppliedTag] =
    val bodyContents = m.docs.fold(Nil)(d =>
      if d.body.getChildren.isEmpty then Seq(d.body) else d.body.getChildren.asScala.toList
    ).map(renderDocPart)

    Seq(
      div(cls := "documentableBrief doc")(bodyContents.take(1)),
      div(cls := "cover")(
        div(cls := "doc")(bodyContents.drop(1)),
        dl(cls := "attributes")(
          docAttributes(m),
          companion(m),
          deprecation(m),
          source(m),
        )
      )
    )