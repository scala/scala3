package dotty.dokka

import org.jetbrains.dokka.plugability.DokkaContext
import org.jetbrains.dokka.pages._
import org.jetbrains.dokka.model._
import org.jetbrains.dokka.base.renderers.html.HtmlRenderer
import org.jetbrains.dokka._
import HTML._
import collection.JavaConverters._
import java.net.URI
import java.net.URL
import java.util.{List => JList, Set => JSet}
import kotlinx.html.FlowContent
import kotlinx.html.stream.StreamKt
import kotlinx.html.Gen_consumer_tagsKt
import kotlinx.html.ApiKt
import kotlinx.html.HTMLTag
import kotlinx.html.DIV
import dotty.dokka.model.api.Link
import dotty.dokka.model.api.HierarchyGraph
import dotty.dokka.model.api.DocPart
import org.jetbrains.dokka.base.resolvers.local.LocationProvider
import org.jetbrains.dokka.base.transformers.pages.comments.DocTagToContentConverter
import dotty.dokka.site.StaticPageNode
import dotty.dokka.site.PartiallyRenderedContent
import scala.util.Try
import org.jetbrains.dokka.base.renderers.html.SearchbarDataInstaller
import org.jsoup.Jsoup
import java.nio.file.Paths

class SignatureRenderer(pageContext: ContentPage, sourceSetRestriciton: JSet[DisplaySourceSet], locationProvider: LocationProvider):
  def link(dri: DRI): Option[String] = Option(locationProvider.resolve(dri, sourceSetRestriciton, pageContext))

  def renderLink(name: String, dri: DRI, modifiers: AppliedAttr*) =
    link(dri) match
      case Some(link) => a(href := link, modifiers)(name)
      case _ => span(Attr("data-unresolved-link") := "", modifiers)(name)

  def renderElementWith(e: String | (String, DRI) | Link, modifiers: AppliedAttr*) = e match
    case (name, dri) => renderLink(name, dri, modifiers:_*)
    case name: String => raw(name)
    case Link(name, dri) => renderLink(name, dri, modifiers:_*)


  def renderElement(e: String | (String, DRI) | Link) = renderElementWith(e)

class ScalaHtmlRenderer(using ctx: DokkaContext) extends HtmlRenderer(ctx) {
  val args = summon[DocContext].args

  // TODO #239
  val hackScalaSearchbarDataInstaller: SearchbarDataInstaller = {
    val f = classOf[HtmlRenderer].getDeclaredField("searchbarDataInstaller")
    f.setAccessible(true)
    f.set(this, ScalaSearchbarDataInstaller(ctx))
    f.get(this).asInstanceOf[ScalaSearchbarDataInstaller]
  }

  // Implementation below is based on Kotlin bytecode and we will try to migrate it to dokka
  // TODO (https://github.com/lampepfl/scala3doc/issues/232): Move this method to dokka
  def withHtml(context: FlowContent, content: String): Unit = context match {
    case tag: HTMLTag =>
      ApiKt.unsafe(tag, { x =>
        x.unaryPlus(content)
        U
      })
    case _ =>
      val div = new DIV(JMap(), context.getConsumer())
      try
        div.getConsumer().onTagStart(div)
        withHtml(div, content)
      catch
        case e: Throwable =>
          div.getConsumer.onTagError(div, e)
      finally
        div.getConsumer.onTagEnd(div)
  }

  lazy val sourceSets = ctx.getConfiguration.getSourceSets.asScala
    .map(s => DisplaySourceSetKt.toDisplaySourceSet(s.asInstanceOf[DokkaConfiguration$DokkaSourceSet])).toSet.asJava

  type FlowContentConsumer = kotlin.jvm.functions.Function1[? >: kotlinx.html.FlowContent, kotlin.Unit]

  override def buildTable(f: FlowContent, node: ContentTable, pageContext: ContentPage, sourceSetRestriciton: JSet[DisplaySourceSet]) = {
    val nodeStyles = node.getStyle.asScala.toSet
    if nodeStyles.contains(TableStyle.DescriptionList) || nodeStyles.contains(TableStyle.NestedDescriptionList) then
      withHtml(f, buildDescriptionList(node, pageContext, sourceSetRestriciton))
    else super.buildTable(f, node, pageContext, sourceSetRestriciton)
  }

  override def wrapGroup(f: FlowContent, node: ContentGroup, pageContext: ContentPage, childrenCallback: FlowContentConsumer) = {
    val additionalClasses = node.getStyle.asScala.map(_.toString.toLowerCase).mkString("", " ", "")
    def buildSymbol: String = div(cls := s"symbol $additionalClasses")(
      raw(
        buildWithKotlinx(childrenCallback).toString
      )
    ).toString
    if node.getDci.getKind == ContentKind.Symbol && node.getStyle.asScala.toSet.contains(TextStyle.Monospace) then withHtml(f, buildSymbol) else super.wrapGroup(f, node, pageContext, childrenCallback)
  }

  override def buildContentNode(f: FlowContent, node: ContentNode, pageContext: ContentPage, sourceSetRestriciton: JSet[DisplaySourceSet]) = {
    node match {
      case n: HtmlContentNode => withHtml(f, raw(n.body).toString)
      case n: HierarchyGraphContentNode => buildDiagram(f, n.diagram, pageContext)
      case n: DocumentableList =>
        val ss = if sourceSetRestriciton == null then Set.empty.asJava else sourceSetRestriciton
        withHtml(f, buildDocumentableList(n, pageContext, ss).toString())
      case n: DocumentableFilter => withHtml(f, buildDocumentableFilter.toString)
      case mi: MemberInfo =>
        val memberHtml = div(renderers(pageContext)._2.memberInfo(mi.member))
        withHtml(f, memberHtml.toString)
      case other => super.buildContentNode(f, node, pageContext, sourceSetRestriciton)
    }
  }

  private def renderers(pageContext: ContentPage): (SignatureRenderer, MemberRenderer) =
    val renderer = SignatureRenderer(pageContext, sourceSets, getLocationProvider)
    (renderer, new MemberRenderer(renderer, buildWithKotlinx(_, pageContext, null)))

  private def buildDocumentableList(n: DocumentableList, pageContext: ContentPage, sourceSetRestriciton: JSet[DisplaySourceSet]) =
    def render(n: ContentNode) = raw(buildWithKotlinx(n, pageContext, null))

    val (renderer, memberRenderer) = renderers(pageContext)
    import renderer._

    def buildDocumentable(element: DocumentableElement) =
      def topLevelAttr = Seq(cls := "documentableElement")
        ++ element.params.dri.anchor.map(id := _)
        ++ element.attributes.map{ case (n, v) => Attr(s"data-f-$n") := v }
      val kind = element.modifiers.takeRight(1)
      val otherModifiers = element.modifiers.dropRight(1)

      val nameStyles = element.nameWithStyles.styles.map(_.toString.toLowerCase).mkString(" ")
      val nameClasses = cls := s"documentableName monospace ${nameStyles.mkString(" ")}"

      div(topLevelAttr:_*)(
        a(href:=link(element.params.dri).getOrElse("#"), cls := "documentableAnchor"),
        div(span(cls := "annotations monospace")(element.annotations.map(renderElement))),
        div(cls := "header")(
          span(cls := "modifiers monospace")(
            span(cls := "other-modifiers")(otherModifiers.map(renderElement)),
            span(cls := "kind")(kind.map(renderElement)),
          ),
          renderLink(element.nameWithStyles.name, element.params.dri, nameClasses),
          span(cls := "signature monospace")(element.signature.map(renderElement)),
        ),
        div(cls := "docs")(
          span(cls := "modifiers monospace"),
          div(
            div(cls := "originInfo")(element.originInfo.map(renderElement)),
            div(cls := "documentableBrief")(memberRenderer.memberInfo(element.member)),
          )
        )
      )

    div(cls := "documentableList", testId := "definitionList")(
      if(n.groupName.isEmpty) raw("") else h3(cls := "documentableHeader")(n.groupName.map(renderElement)),
      n.elements.flatMap {
        case element: DocumentableElement =>
          Seq(buildDocumentable(element))
        case group: DocumentableElementGroup =>
          h4(cls := "documentable-extension-target")(
            group.header.map(renderElement)
          ) +: group.elements.map(buildDocumentable)
    }
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

  def buildDescriptionList(node: ContentTable, pageContext: ContentPage, sourceSetRestriciton: JSet[DisplaySourceSet]) = {
    val children = node.getChildren.asScala.toList.zipWithIndex
    val nodeStyles = node.getStyle.asScala.toSet
    val classes = if nodeStyles.contains(TableStyle.NestedDescriptionList) then "paramsdesc" else "attributes"
    dl(cls := classes)(
      children.map((e, i) =>
        if(i % 2 == 0)
          dt(
            raw(
              buildWithKotlinx(e, pageContext, sourceSetRestriciton)
            )
          )
        else
          dd(
            raw(
              buildWithKotlinx(e, pageContext, sourceSetRestriciton)
            )
          )
      )
    ).toString
  }

  override def buildResolvedLink(
    f: FlowContent,
    node: ContentResolvedLink,
    pageContext: ContentPage,
    sourceSetRestriction: JSet[DisplaySourceSet],
  ): Unit = {
    import kotlinx.html.{Gen_consumer_tagsKt => dsl}
    val c = f.getConsumer

    dsl.a(c, node.getAddress, /*target*/ null, /*classes*/ null, { e =>
      import ScalaCommentToContentConverter._
      // node.getExtra.getMap.asScala.get(LinkAttributesKey)
      Option(node.get(LinkAttributesKey).asInstanceOf[ExtraLinkAttributes])
        .flatMap(_.title)
        .foreach(e.getAttributes.put("title", _))
      buildText(f, node.getChildren, pageContext, sourceSetRestriction)
      U
    })
  }

  override def buildCodeBlock(
    f: FlowContent,
    code: ContentCodeBlock,
    pageContext: ContentPage,
  ): Unit = {
    // we cannot use Scalatags, because we need to call buildContentNode
    // TODO rewrite it to using HTML
    import kotlinx.html.{Gen_consumer_tagsKt => dsl}
    val c = f.getConsumer

    dsl.div(c, "sample-container", { e =>
      dsl.pre(c, null, { e =>
        val codeClass = code.getStyle.asScala.iterator.map(_.toString.toLowerCase).mkString("", " ", " language-scala")
        dsl.code(c, codeClass, { e =>
          e.getAttributes.put("theme", "idea")
          code.getChildren.asScala.foreach(buildContentNode(f, _, pageContext, /*sourceSetRestriction*/ null))
          U
        })
        U
      })
      U
    })
  }

  def buildDiagram(f: FlowContent, diagram: HierarchyGraph, pageContext: ContentPage) =
    val renderer = SignatureRenderer(pageContext, sourceSets, getLocationProvider)
    withHtml(f, div( id := "inheritance-diagram", cls := "diagram-class")(
        svg(id := "graph"),
        script(`type` := "text/dot", id := "dot")(raw(DotDiagramBuilder.build(diagram, renderer))),
      ).toString()
    )

  private val HashRegex = "([^#]+)(#.+)".r

  override def buildPageContent(context: FlowContent, page: ContentPage): Unit =
    page match
      case s: StaticPageNode if !s.hasFrame() =>
      case _ => buildNavigation(context, page)

    page.getContent match
      case prc: PartiallyRenderedContent =>
        def tryAsDri(str: String) =
          val (path, prefix) = str match
            case HashRegex(path, prefix) => (path, prefix)
            case _ => (str, "")

          val dri = prc.context.driForLink(prc.template.templateFile, path)
          val res = dri.flatMap(dri => Option(getLocationProvider.resolve(dri, sourceSets, page)))
          if res.isEmpty then
            report.warn(s"Unable to resolve link '$str'", prc.template.file)
          res.headOption.fold(str)(_ + prefix)

        def processLocalLink(str: String): String =
          if str.startsWith("#") || str.isEmpty then str
          else Try(URL(str)).map(_ => str).getOrElse(tryAsDri(str))

        val html = prc.procsesHtml(processLocalLink, resolveLink(page))
        withHtml(context, html)
      case content =>
        build(content, context, page, /*sourceSetRestriction=*/null)


  override def buildHtml(page: PageNode, resources: JList[String], kotlinxContent: FlowContentConsumer): String =
    val (pageTitle, noFrame) = page match
      case static: StaticPageNode =>
        (static.template.title, !static.hasFrame())
      case _ =>
        (page.getName, false)

    val projectLogo =
      args.projectLogo.map { path =>
        val fileName = Paths.get(path).getFileName()
        span(img(src := resolveRoot(page, s"project-logo/$fileName")))
      }.toSeq

    html(
      head(
        meta(charset := "utf-8"),
        meta(name := "viewport", content := "width=device-width, initial-scale=1"),
        title(pageTitle),
        link(rel := "shortcut icon", `type` := "image/x-icon", href := resolveLink(page)("favicon.ico")),
        linkResources(page, resources.asScala).toSeq,
        script(raw(s"""var pathToRoot = "${getLocationProvider.pathToRoot(page)}";"""))
      ),
      body(
        if noFrame then raw(buildWithKotlinx(kotlinxContent)) else
          div(id := "container")(
            div(id := "leftColumn")(
              div(id := "logo")(
                projectLogo,
                span(
                  div(cls:="projectName")(args.name)
                ),
                span(
                  args.projectVersion.map(v => div(cls:="projectVersion")(v)).toList
                )
              ),
              div(id := "paneSearch"),
              nav(id := "sideMenu"),
            ),
            div(id := "main")(
              div (id := "leftToggler")(
                span(cls := "icon-toggler")
              ),
              div(id := "searchBar"),
              main(
                raw(buildWithKotlinx(kotlinxContent))
              ),
              footer(
                span(cls := "go-to-top-icon")(
                  a(href := "#container")(
                    span(cls:="icon-vertical_align_top"),
                    raw("&nbsp;Back to top")
                  )
                ),
                raw("Generated by&nbsp;"),
                a(href := "https://github.com/lampepfl/dotty/tree/master/scala3doc")(
                  img(src := resolveRoot(page, "images/scala3doc_logo.svg"), alt := "Scala3doc", cls := "scala3doc_logo")
                )
              )
            )
          ),
          script(`type` := "text/javascript", src := resolveRoot(page, "scripts/pages.js")),
          script(`type` := "text/javascript", src := resolveRoot(page, "scripts/main.js"))
      )
    ).toString

  private def resolveRoot(page: PageNode, path: String) =
    getLocationProvider.pathToRoot(page) + path

  private def resolveLink(page: PageNode)(url: String): String =
    if URI(url).isAbsolute then url else resolveRoot(page, url)

  private def linkResources(page: PageNode, resources: Iterable[String]): Iterable[AppliedTag] =
    def fileExtension(url: String): String =
      val param = url.indexOf('?')
      val end = if param < 0 then url.length else param
      val point = url.lastIndexOf('.', end)
      url.substring(point+1, end)

    for res <- resources yield
      fileExtension(res) match
        case "css" => link(rel := "stylesheet", href := resolveLink(page)(res))
        case "js" => script(`type` := "text/javascript", src := resolveLink(page)(res), defer := "true")
        case _ => raw(res)

  private def buildWithKotlinx(node: ContentNode, pageContext: ContentPage, sourceSetRestriction: JSet[DisplaySourceSet]): String =
    Gen_consumer_tagsKt.div(
      StreamKt.createHTML(true, false),
      null,
      (div) => {build(node, div, pageContext, sourceSetRestriction); kotlin.Unit.INSTANCE}
    ).toString.stripPrefix("<div>").stripSuffix("</div>\n")

  private def buildWithKotlinx(func: FlowContentConsumer): String =
    Gen_consumer_tagsKt.div(
      StreamKt.createHTML(true, false),
      null,
      func
    ).toString.stripPrefix("<div>").stripSuffix("</div>\n")
}
