package dotty.dokka

import dotty.dokka.model.api._
import scala.collection.immutable.SortedMap
import dotty.dokka.HTML._
import org.jetbrains.dokka.pages.{DCI, ContentKind, ContentNode}
import org.jetbrains.dokka.model.properties.PropertyContainer
import collection.JavaConverters._
import dotty.dokka.translators.FilterAttributes

class MemberRenderer(signatureRenderer: SignatureRenderer, buildNode: ContentNode => String)(using DocContext):

  import signatureRenderer._
  private val converter = ScalaCommentToContentConverter

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

  def doc(m: Member): Seq[AppliedTag] =  m.docs.fold(Nil)(d => Seq(renderDocPart(d.body)))

  def tableRow(name: String, content: AppliedTag) = Seq(dt(name), dd(content))

  def defintionClasses(m: Member) = m.origin match
    case Origin.Overrides(defs) =>
      def renderDef(d: Overriden): Seq[TagArg] =
        Seq(" -> ", signatureRenderer.renderLink(d.name, d.dri))
      val headNode = m.inheritedFrom.map(form => signatureRenderer.renderLink(form.name, form.dri))
      val tailNodes = defs.flatMap(renderDef)
      val nodes = headNode.fold(tailNodes.drop(1))(_ +: tailNodes)
      tableRow("Definition Classes", div(nodes:_*))

    case _ => Nil

  def inheritedFrom(m: Member) = m.inheritedFrom match
    case Some(InheritedFrom(name, dri)) => tableRow("Inhertied from", signatureRenderer.renderLink(name, dri))
    case _ => Nil

  def docAttributes(m: Member): Seq[AppliedTag] =

    def nested(name: String, on: SortedMap[String, DocPart]): Seq[AppliedTag] =
      if on.isEmpty then Nil else
        tableRow(name, dl(cls := "attributes")(
          on.map { case (name, value) => tableRow(name, renderDocPart(value))}.toList:_*
        ))

    def list(name: String, on: List[DocPart]): Seq[AppliedTag] =
      if on.isEmpty then Nil else tableRow(name, div(on.map(e => div(renderDocPart(e)))))

    def opt(name: String, on: Option[DocPart]): Seq[AppliedTag] =
      if on.isEmpty then Nil else tableRow(name, renderDocPart(on.get))

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
          defintionClasses(m),
          inheritedFrom(m),
          source(m),
        )
      )
    )

  private def originInfo(m: Member): Seq[TagArg] = m.origin match {
    case Origin.ImplicitlyAddedBy(name, dri) =>
      Seq("Implicitly added by ", renderLink(name, dri))
    case Origin.ExtensionFrom(name, dri) =>
      Seq("Extension method from ", renderLink(name, dri))
    case Origin.ExportedFrom(name, dri) =>
      val signatureName: TagArg = dri match
        case Some(dri: DRI) => renderLink(name, dri)
        case None => name
      Seq("Exported from ", signatureName)
    case _ => Nil
  }

  def memberSingnature(member: Member) =
    val depStyle = if member.deprecated.isEmpty then "" else "deprecated"
    val nameClasses = cls := s"documentableName $depStyle"

    val rawBuilder = ScalaSignatureProvider.rawSignature(member, InlineSignatureBuilder())
    val inlineBuilder = rawBuilder.asInstanceOf[InlineSignatureBuilder]
    if inlineBuilder.preName.isEmpty then println(member)
    val kind :: modifiersRevered = inlineBuilder.preName
    val signature = inlineBuilder.names.reverse
    Seq(
      span(cls := "modifiers")(
        span(cls := "other-modifiers")(modifiersRevered.reverse.map(renderElement)),
        span(cls := "kind")(renderElement(kind)),
      ),
      renderLink(member.name, member.dri, nameClasses),
      span(cls := "signature")(signature.map(renderElement)),
    )

  def annotations(member: Member) =
   val rawBuilder = InlineSignatureBuilder().annotationsBlock(member)
   val signatures = rawBuilder.asInstanceOf[InlineSignatureBuilder].names.reverse
   span(cls := "annotations monospace")(signatures.map(renderElement))

  def member(member: Member) =
    val filterAttributes = FilterAttributes.attributesFor(member)
    val anchor = if member.dri.anchor.isEmpty then Nil else Seq(id := member.dri.anchor)
    def topLevelAttr = Seq(cls := "documentableElement")
      ++ anchor
      ++ filterAttributes.map{ case (n, v) => Attr(s"data-f-$n") := v }

    div(topLevelAttr:_*)(
      a(href:=link(member.dri).getOrElse("#"), cls := "documentableAnchor"),
      div(annotations(member)),
      div(cls := "header monospace")(memberSingnature(member)),
      div(cls := "docs")(
        span(cls := "modifiers"), // just to have padding on left
        div(
          div(cls := "originInfo")(originInfo(member):_*),
          div(cls := "documentableBrief")(memberInfo(member)),
        )
      )
    )

  private case class MGroup(header: AppliedTag, members: Seq[Member])

  private def actualGroup(name: String, members: Seq[Member | MGroup]): Seq[AppliedTag] =
    if members.isEmpty then Nil else
    div(cls := "documentableList")(
      h3(cls:="groupHeader")(name),
      members.map {
        case element: Member =>
          member(element)
        case MGroup(header, members) =>
          div(
            header,
            members.map(member)
          )
      }
    ) :: Nil

  private def isDeprecated(m: Member | MGroup): Boolean = m match
    case m: Member => m.deprecated.nonEmpty
    case g: MGroup => g.members.exists(isDeprecated)

  private def isInherited(m: Member | MGroup): Boolean = m match
    case m: Member => m.inheritedFrom.nonEmpty
    case g: MGroup => g.members.exists(isInherited)

  private type SubGroup = (String, Seq[Member | MGroup])
  private def buildGroup(name: String, subgroups: Seq[SubGroup]): Tab =
    val all = subgroups.map { case (name, members) =>
      val (allInherited, allDefined) = members.partition(isInherited)
      val (depDefined, defined) = allDefined.partition(isDeprecated)
      val (depInherited, inherited) = allInherited.partition(isDeprecated)
      (
        actualGroup(name, defined),
        actualGroup(s"Deprectated ${name.toLowerCase}", depDefined),
        actualGroup(s"Inherited ${name.toLowerCase}", inherited),
        actualGroup(s"Deprectated and Inherited ${name.toLowerCase}", depInherited)
      )
    }

    val children =
      all.flatMap(_._1) ++ all.flatMap(_._2) ++ all.flatMap(_._3) ++ all.flatMap(_._4)
    if children.isEmpty then emptyTab
    else Tab(name, name, h2(tabAttr(name))(name) +: children, "selected")

  case class ExpandedGroup(name: AppliedTag, description: AppliedTag, prio: Int)

  val emptyTab = Tab("", "", Nil)

  def grouppedMember(m: Member, membersInGroups: Seq[Member]): Tab =
    if membersInGroups.isEmpty then emptyTab else
      val descriptions = m.docs.map(_.groupDesc).getOrElse(Map.empty)
      val names = m.docs.map(_.groupNames).getOrElse(Map.empty)
      val prios = m.docs.map(_.groupPrio).getOrElse(Map.empty)

      val rawGroups = membersInGroups.groupBy(_.docs.flatMap(_.group)).collect {
          case (Some(groupName), members) =>
            ExpandedGroup(
              names.get(groupName).fold(raw(groupName))(renderDocPart),
              descriptions.get(groupName).fold(raw(""))(renderDocPart),
              prios.getOrElse(groupName, 1000)
            ) -> members
        }
      val content = rawGroups.toSeq.sortBy(_._1.prio).flatMap {
        case (group, members) =>
          Seq(div(cls := "documentableList")(
            h3(group.name),
            group.description,
            members.map(member)
          ))
      }
      Tab("Grouped members", "custom_groups", content, "selected")

  def buildMembers(s: Member): AppliedTag =
    val (membersInGroups, rest) = s.members.partition(_.docs.exists(_.group.nonEmpty))

    val extensions =
      rest.groupBy{ _.kind match
        case Kind.Extension(on, _) => Some(on)
        case _ => None
      }.collect {
        case (Some(on), members) =>
          val sig = Signature(s"extension (${on.name}: ") ++ on.signature ++ Signature(")")
          MGroup(span(sig.map(renderElement)), members.toSeq)
      }.toSeq

    div(cls := "membersList")(renderTabs(
      singleSelection = false,
      buildGroup("Packages", Seq(
        ("", rest.filter(m => m.kind == Kind.Package)),
      )),
      grouppedMember(s, membersInGroups),
      buildGroup("Type members", Seq(
        ("Classlikes", rest.filter(m => m.kind.isInstanceOf[Classlike])),
        ("Types", rest.filter(_.kind.isInstanceOf[Kind.Type])),
        ("Enum entries", rest.filter(_.kind == Kind.EnumCase)),
      )),
      buildGroup("Value members", Seq(
        ("Constructors", rest.filter(_.kind.isInstanceOf[Kind.Constructor])),
        ("Methods", rest.filter(_.kind.isInstanceOf[Kind.Def])),
        ("Fields", rest.filter(m => m.kind == Kind.Val || m.kind == Kind.Var)),
      )),
      buildGroup("Givens", Seq(
        ("Givens", rest.filter(_.kind.isInstanceOf[Kind.Given])),
      )),
      buildGroup("Extensions", Seq(
        ("Extensions", extensions),
      )),
      buildGroup("Implicits", Seq(
        ("Implicits", rest.filter(_.kind.isInstanceOf[Kind.Implicit])),
      )),
      buildGroup("Exports", Seq(
        ("Defined exports", rest.filter(_.kind.isInstanceOf[Kind.Exported])),
      ))
    ))

  case class Tab(name: String, id: String, content: Seq[AppliedTag], cls: String = "")

  def tabAttr(id: String) = Attr("data-togglable") := id

  private def renderTabs(singleSelection: Boolean, allTabs: Tab*): Seq[AppliedTag] =
    val tabs = allTabs.filter(_.content.nonEmpty)
      if tabs.isEmpty then Nil else
        Seq(div(cls := (if singleSelection then "tabs single" else "tabs"))(
            div(cls := "names")(tabs.map(t =>
              button(tabAttr(t.id), cls := s"tab ${t.cls}")(t.name)
            )),
            div(cls := "contents")(tabs.map(t =>
              div(tabAttr(t.id), cls := s"tab ${t.cls}")(t.content)
            ))
        ))

  def classLikeParts(m: Member): Seq[AppliedTag] =
    if !m.kind.isInstanceOf[Classlike] then Nil else
      val graphHtml = m.graph match
        case graph if graph.edges.nonEmpty =>
          Seq(div( id := "inheritance-diagram", cls := "diagram-class showGraph")(
            input(value := "Reset zoom", `type` := "button", cls := "btn", onclick := "zoomOut()"),
            svg(id := "graph"),
            script(`type` := "text/dot", id := "dot")(
              raw(DotDiagramBuilder.build(graph, signatureRenderer))
            ),
          ))
        case _ => Nil

      def signatureList(list: Seq[LinkToType]): Seq[AppliedTag] =
        if list.isEmpty then Nil
        else Seq(div(cls := "symbol monospace")(list.map(link =>
          div(link.kind.name," ", link.signature.map(renderElement))
        )))

      val supertypes = signatureList(m.parents)
      val subtypes = signatureList(m.knownChildren)

      renderTabs(
        singleSelection = true,
        Tab("Graph", "graph", graphHtml, "showGraph"),
        Tab("Supertypes", "supertypes", supertypes),
        Tab("Known subtypes", "subtypes", subtypes),
      )

  private def buildDocumentableFilter = div(cls := "documentableFilter")(
    div(cls := "filterUpperContainer")(
      button(cls := "filterToggleButton", testId := "filterToggleButton")(
        raw("""
          <svg xmlns="http://www.w3.org/2000/svg" height="24" viewBox="0 0 24 24" width="24">
            <path d="M0 0h24v24H0z" fill="none"/>
            <path d="M10 6L8.59 7.41 13.17 12l-4.58 4.59L10 18l6-6z"/>
          </svg>
        """)
      ),
      input(cls := "filterableInput", placeholder := "Filter all members", testId := "filterBarInput")
    ),
    div(cls := "filterLowerContainer")()
  )

  def fullMember(m: Member): AppliedTag =
    val intro = m.kind match
      case Kind.RootPackage =>Seq(h1(summon[DocContext].args.name))
      case _ =>
        Seq(
          h1(m.name),
          div(cls:= "header monospace")(annotations(m), memberSingnature(m))
        )

    div(
      intro,
      memberInfo(m),
      classLikeParts(m),
      buildDocumentableFilter, // TODO Need to make it work in JS :(
      buildMembers(m))
