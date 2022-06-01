package dotty.tools.scaladoc
package renderers

import scala.collection.immutable.SortedMap
import util.HTML._
import collection.JavaConverters._
import dotty.tools.scaladoc.translators.FilterAttributes
import dotty.tools.scaladoc.tasty.comments.markdown.DocFlexmarkRenderer
import com.vladsch.flexmark.util.ast.{Node => MdNode}
import dotty.tools.scaladoc.tasty.comments.wiki.WikiDocElement
import translators._

class MemberRenderer(signatureRenderer: SignatureRenderer)(using DocContext) extends DocRender(signatureRenderer):
  import signatureRenderer._

  def doc(m: Member): Seq[AppliedTag] =  m.docs.fold(Nil)(d => Seq(renderDocPart(d.body)))

  def tableRow(name: String, content: TagArg) = Seq(dt(name), dd(content))

  def defintionClasses(m: Member) = m.origin match
    case Origin.Overrides(defs) =>
      def renderDef(d: Overridden): Seq[TagArg] =
        Seq(" -> ", signatureRenderer.renderLink(d.name, d.dri))
      val headNode = m.inheritedFrom.map(form => signatureRenderer.renderLink(form.name, form.dri))
      val tailNodes = defs.flatMap(renderDef)
      val nodes = headNode.fold(tailNodes.drop(1))(_ +: tailNodes)
      tableRow("Definition Classes", div(nodes:_*))

    case _ => Nil

  def inheritedFrom(m: Member) = m.inheritedFrom match
    case Some(InheritedFrom(name, dri, isSourceSuperclassHidden)) =>
      val hiddenNameSuffix = if isSourceSuperclassHidden then " (hidden)" else ""
      tableRow("Inherited from:", signatureRenderer.renderLink(name + hiddenNameSuffix, dri))
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

    def authors(authors: List[DocPart]) = if summon[DocContext].args.includeAuthors then list("Authors:", authors) else Nil

    m.docs.fold(Nil)(d =>
      nested("Type parameters:", d.typeParams) ++
      nested("Value parameters:", d.valueParams) ++
      opt("Returns:", d.result) ++
      nested("Throws:", d.throws) ++
      opt("Constructor:", d.constructor) ++
      authors(d.authors) ++
      list("See also:", d.see) ++
      opt("Version:", d.version) ++
      opt("Since:", d.since) ++
      list("Todo:", d.todo) ++
      list("Note:", d.note) ++
      list("Example:", d.example)
    )

  def companion(m: Member): Seq[AppliedTag] = m.companion.fold(Nil){dri =>
    val kindName = if m.kind == Kind.Object then "class" else "object"
    tableRow("Companion:", signatureRenderer.renderLink(kindName, dri))
  }

  def source(m: Member): Seq[AppliedTag] =
    summon[DocContext].sourceLinks.pathTo(m).fold(Nil){ link =>
      tableRow("Source:", a(href := link)(m.sources.fold("(source)")(_.path.getFileName().toString())))
    }

  def deprecation(m: Member): Seq[AppliedTag] = m.deprecated.fold(Nil){ a =>
    def stripQuotes(s: String) = s.stripPrefix("\"").stripSuffix("\"")
    def parameter(p: Annotation.AnnotationParameter): TagArg = p match
      case Annotation.PrimitiveParameter(_, value) => stripQuotes(value)
      case Annotation.LinkParameter(_, dri, text) =>
        signatureRenderer.renderLink(stripQuotes(text), dri)
      case Annotation.UnresolvedParameter(_, value) => stripQuotes(value)

    val (named, unnamed) = a.params.partition(_.name.nonEmpty)
    val message = named.find(_.name.get == "message")
    val since = named.find(_.name.get == "since")

    val content = (
      Seq(
        since.map(s => code("[Since version ", parameter(s), "] ")),
        message.map(m => parameter(m)))
      ++ m.docs.map(_.deprecated.toSeq.map(renderDocPart))
    ).flatten
    Seq(dt("Deprecated"), dd(content:_*))
  }

  def memberInfo(m: Member, withBrief: Boolean = false): Seq[AppliedTag] =
    val comment = m.docs
    val bodyContents = m.docs.fold(Nil)(e => renderDocPart(e.body) :: Nil)

    val classLikeInfo: TagArg = classLikeParts(m)

    Seq(
      Option.when(withBrief)(div(cls := "documentableBrief doc")(comment.flatMap(_.short).fold("")(renderDocPart))),
      Some(
        div(cls := "cover")(
          div(cls := "doc")(bodyContents),
          dl(cls := "attributes")(
            docAttributes(m),
            companion(m),
            deprecation(m),
            defintionClasses(m),
            inheritedFrom(m),
            source(m),
            classLikeInfo
          )
        )
      )
    ).flatten

  private def originInfo(m: Member): Seq[TagArg] = m.origin match {
    case Origin.ImplicitlyAddedBy(name, dri) =>
      Seq("Implicitly added by ", renderLink(name, dri))
    case Origin.ExtensionFrom(name, dri) =>
      Seq("Extension method from ", renderLink(name, dri))
    case Origin.ExportedFrom(Some(link)) =>
      val signatureName: TagArg = link match
        case Link(name, dri) => renderLink(name, dri)
      Seq("Exported from ", signatureName)
    case _ => Nil
  }

  def memberSignature(member: Member) =
    val depStyle = if member.deprecated.isEmpty then "" else "deprecated"
    val nameClasses = Seq(
      cls := s"documentableName $depStyle",
      Attr("t") := "n"
    )

    val rawBuilder = ScalaSignatureProvider.rawSignature(member, InlineSignatureBuilder())()
    val inlineBuilder = rawBuilder.asInstanceOf[InlineSignatureBuilder]
    val kind :: modifiersRevered = inlineBuilder.preName: @unchecked
    val signature = inlineBuilder.names.reverse
    Seq(
      div(cls := "signature")(
        span(cls := "modifiers")(modifiersRevered.reverse.map(renderElement)),
        span(cls := "kind")(renderElement(kind)),
        renderLink(member.name, member.dri, nameClasses*),
        span(signature.map(renderElement))
      ),
    )

  def memberIcon(member: Member) = member.kind match {
    case _ =>
      val withCompanion = member.companion.fold("")(_ => "-wc")
      val iconSpan = span(cls := s"micon ${member.kind.name.take(2)}$withCompanion")()
      Seq(member.companion.flatMap(link(_)).fold(iconSpan)(link => a(href := link)(iconSpan)))
  }

  def annotations(member: Member) =
   val rawBuilder = InlineSignatureBuilder().annotationsBlock(member)
   val signatures = rawBuilder.asInstanceOf[InlineSignatureBuilder].names.reverse
   span(cls := "annotations monospace")(signatures.map(renderElement))

  def member(member: Member) =
    val filterAttributes = FilterAttributes.attributesFor(member)
    val anchor = if member.dri.anchor.isEmpty then Nil else Seq(id := member.dri.anchor)
    def topLevelAttr = Seq(cls := "documentableElement mono-small-inline")
      ++ anchor
      ++ filterAttributes.map{ case (n, v) => Attr(s"data-f-$n") := v }

    div(topLevelAttr:_*)(
      button(cls := "icon-button show-content"),
      if !member.needsOwnPage then a(Attr("link") := link(member.dri).getOrElse("#"), cls := "documentableAnchor") else Nil,
      div(annotations(member)),
      div(cls := "header monospace")(memberSignature(member)),
      div(cls := "docs")(
        span(cls := "modifiers"), // just to have padding on left
        div(
          div(cls := "originInfo")(originInfo(member):_*),
          div(cls := "memberDocumentation")(memberInfo(member, withBrief = true)),
        )
      )
    )

  private case class MGroup(header: AppliedTag, members: Seq[Member], groupName: String)

  private def actualGroup(name: String, members: Seq[Member | MGroup]): Seq[AppliedTag] =
    if members.isEmpty then Nil else
    div(cls := "documentableList")(
      h3(cls:="groupHeader")(name),
      members.sortBy {
        case m: Member => m.name
        case MGroup(_, _, name) => name
      }.map {
        case element: Member =>
          member(element)
        case MGroup(header, members, _) =>
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

  private def isAbstract(m: Member | MGroup): Boolean = m match
    case m: Member => m.modifiers.exists(Set(Modifier.Abstract, Modifier.Deferred).contains)
    case g: MGroup => g.members.exists(isAbstract)

  private type SubGroup = (String, Seq[Member | MGroup])
  private def buildGroup(name: String, subgroups: Seq[SubGroup]): Tab =
    val all = subgroups.map { case (name, members) =>
      val (allInherited, allDefined) = members.partition(isInherited)
      val (depDefined, defined) = allDefined.partition(isDeprecated)
      val (depInherited, inherited) = allInherited.partition(isDeprecated)
      val normalizedName = name.toLowerCase
      val definedWithGroup = if Set("methods", "fields").contains(normalizedName) then
          val (abstr, concr) = defined.partition(isAbstract)
          Seq(
            actualGroup(s"Abstract ${normalizedName}", abstr),
            actualGroup(s"Concrete ${normalizedName}", concr)
          )
        else
          Seq(actualGroup(name, defined))

      definedWithGroup ++ List(
        actualGroup(s"Deprecated ${normalizedName}", depDefined),
        actualGroup(s"Inherited ${normalizedName}", inherited),
        actualGroup(s"Deprecated and Inherited ${normalizedName}", depInherited)
      )
    }

    val children = all.flatten.flatten
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
    def partitionIntoGroups(members: Seq[Member]) =
      if summon[DocContext].args.includeGroups then
        members.partition(_.docs.exists(_.group.nonEmpty))
      else
        (Nil, members)

    val (membersInGroups, rest) = partitionIntoGroups(s.members)

    val extensions =
      rest.groupBy{ _.kind match
        case Kind.Extension(on, _) => Some(on)
        case _ => None
      }.collect {
        case (Some(on), members) =>
          val typeSig = InlineSignatureBuilder()
            .keyword("extension ")
            .generics(on.typeParams)
            .asInstanceOf[InlineSignatureBuilder].names.reverse
          val argsSig = InlineSignatureBuilder()
            .functionParameters(on.argsLists)
            .asInstanceOf[InlineSignatureBuilder].names.reverse
          val sig = typeSig ++ Signature(Plain(s"(${on.name}: ")) ++ on.signature ++ Signature(Plain(")")) ++ argsSig
          MGroup(span(cls := "groupHeader")(sig.map(renderElement)), members.sortBy(_.name).toSeq, on.name)
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
        ("Enum entries", rest.filter(_.kind.isInstanceOf[Kind.EnumCase])),
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

  def classLikeParts(m: Member): TagArg =
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
         else Seq(div(cls := "mono-small-inline")(list.map(link =>
          div(link.kind.name," ", link.signature.map(renderElement))
        )))

      def selfTypeList(list: List[LinkToType]): Seq[AppliedTag] =
        if list.isEmpty then Nil
        else Seq(div(cls := "mono-small-inline") { list.map { link =>
          div(link.signature.map(renderElement))
        }})

      val supertypes = signatureList(m.parents)
      val subtypes = signatureList(m.knownChildren)
      val selfType = selfTypeList(m.selfType.toList)

      Seq(
        "Graph" -> graphHtml,
        "Supertypes" -> supertypes,
        "Known subtypes" -> subtypes,
        "Self type" -> selfType,
      ).filterNot(_._2.isEmpty).flatMap(tableRow(_, _))

  private def renderTable(keyValues: (TagArg, TagArg)*): TagArg =
    table(
      keyValues.map((key, value) => tr(td(key), td(value)))
    )

  private def buildDocumentableFilter = div(cls := "documentableFilter")(
    div(cls := "filtersContainer")(),
    input(cls := "filterableInput", placeholder := "Filter by any phrase", testId := "filterBarInput", `type` := "search"),
    button(cls := "clearButton label-only-button",  testId := "filterBarClearButton")("Clear all"),
  )

  def fullMember(m: Member): PageContent =
    val intro = m.kind match
      case Kind.RootPackage =>Seq(h1(summon[DocContext].args.name))
      case _ =>
        Seq(
          div(cls := "cover-header")(
            memberIcon(m),
            h1(cls := "h600")(m.name)
          ),
          div(cls := "signature monospace")(
            annotations(m),
            memberSignature(m)
          )
        )
    PageContent(
      div(
        buildDocumentableFilter,
        intro,
        memberInfo(m, withBrief = false),
        buildMembers(m)
      ),
      Seq.empty // For now, we don't support table of contents in members
    )
