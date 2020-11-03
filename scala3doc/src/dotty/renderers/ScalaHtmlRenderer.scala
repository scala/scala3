package dotty.dokka

import org.jetbrains.dokka.plugability.DokkaContext
import org.jetbrains.dokka.pages._
import org.jetbrains.dokka.model._
import org.jetbrains.dokka.base.renderers.html.HtmlRenderer
import org.jetbrains.dokka._
import HTML._
import collection.JavaConverters._
import java.net.URI
import java.util.{List => JList, Set => JSet}
import kotlinx.html.FlowContent
import kotlinx.html.stream.StreamKt
import kotlinx.html.Gen_consumer_tagsKt
import kotlinx.html.ApiKt
import kotlinx.html.HTMLTag
import kotlinx.html.DIV
import org.jetbrains.dokka.links.DRI
import dotty.dokka.model.api.Link
import dotty.dokka.model.api.HierarchyGraph
import org.jetbrains.dokka.base.resolvers.local.LocationProvider
import dotty.dokka.site.StaticPageNode
import dotty.dokka.site.PartiallyRenderedContent

class SignatureRenderer(pageContext: ContentPage, sourceSetRestriciton: JSet[DisplaySourceSet], locationProvider: LocationProvider):
  def link(dri: DRI): Option[String] = Option(locationProvider.resolve(dri, sourceSetRestriciton, pageContext))

  def renderLink(name: String, dri: DRI, modifiers: AppliedAttr*) =
    link(dri) match
      case Some(link) => a(href := link, modifiers)(name)
      case _ => span(Attr("data-unresolved-link") := dri.toString, modifiers)(name)

  def renderElementWith(e: String | (String, DRI) | Link, modifiers: AppliedAttr*) = e match
    case (name, dri) => renderLink(name, dri, modifiers:_*)
    case name: String => raw(name)
    case Link(name, dri) => renderLink(name, dri, modifiers:_*)


  def renderElement(e: String | (String, DRI) | Link) = renderElementWith(e)

class ScalaHtmlRenderer(ctx: DokkaContext) extends HtmlRenderer(ctx) {

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
    val additionalClasses = node.getStyle.asScala.map(_.toString.toLowerCase).mkString("", ",", "")
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
      case other => super.buildContentNode(f, node, pageContext, sourceSetRestriciton)
    }
  }

  private val anchor = raw("""
    <svg width="24" height="24" viewBox="0 0 24 24" fill="darkgray" xmlns="http://www.w3.org/2000/svg">
      <path d="M21.2496 5.3C20.3496 4.5 19.2496 4 18.0496 4C16.8496 4 15.6496 4.5 14.8496 5.3L10.3496 9.8L11.7496 11.2L16.2496 6.7C17.2496 5.7 18.8496 5.7 19.8496 6.7C20.8496 7.7 20.8496 9.3 19.8496 10.3L15.3496 14.8L16.7496 16.2L21.2496 11.7C22.1496 10.8 22.5496 9.7 22.5496 8.5C22.5496 7.3 22.1496 6.2 21.2496 5.3Z"></path>
      <path d="M8.35 16.7998C7.35 17.7998 5.75 17.7998 4.75 16.7998C3.75 15.7998 3.75 14.1998 4.75 13.1998L9.25 8.6998L7.85 7.2998L3.35 11.7998C1.55 13.5998 1.55 16.3998 3.35 18.1998C4.25 19.0998 5.35 19.4998 6.55 19.4998C7.75 19.4998 8.85 19.0998 9.75 18.1998L14.25 13.6998L12.85 12.2998L8.35 16.7998Z"></path>
    </svg>
  """)



  private def buildDocumentableList(n: DocumentableList, pageContext: ContentPage, sourceSetRestriciton: JSet[DisplaySourceSet]) =
    def render(n: ContentNode) = raw(buildWithKotlinx(n, pageContext, null))

    val renderer = SignatureRenderer(pageContext, sourceSets, getLocationProvider)
    import renderer._

    def buildDocumentable(element: DocumentableElement) =
      def topLevelAttr = Seq(cls := "documentableElement") ++ element.attributes.map{ case (n, v) => Attr(s"data-f-$n") := v }
      val kind = element.modifiers.takeRight(1)
      val otherModifiers = element.modifiers.dropRight(1)

      div(topLevelAttr:_*)(
        div(cls := "annotations monospace")(element.annotations.map(renderElement)),
        div(
          a(href:=link(element.params.dri).getOrElse("#"), cls := "documentableAnchor")(anchor),
          span(cls := "modifiers monospace")(
            span(cls := "other-modifiers")(otherModifiers.map(renderElement)),
            span(cls := "kind")(kind.map(renderElement)),
          ),
          renderLink(element.name, element.params.dri, cls := "documentableName monospace"),
          span(cls := "signature monospace")(element.signature.map(renderElement)),
          div(
            div(cls := "originInfo")(element.originInfo.map(renderElement)),
            div(cls := "documentableBrief")(element.brief.map(render)),
          )
        ),

      )

    div(cls := "documentableList")(
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
      button(cls := "filterToggleButton")(
        raw("""
          <svg xmlns="http://www.w3.org/2000/svg" height="24" viewBox="0 0 24 24" width="24">
            <path d="M0 0h24v24H0z" fill="none"/>
            <path d="M10 6L8.59 7.41 13.17 12l-4.58 4.59L10 18l6-6z"/>
          </svg>
        """)
      ),
      input(cls := "filterableInput", placeholder := "Filter all members")
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

  private def render(c: PartiallyRenderedContent, p: ContentPage): String  =
      val parsed = if (!c.page.hasMarkdown) c.page.code else
        div(raw(
          buildWithKotlinx{ div => 
            c.getChildren.forEach(build(_, div, p, /*sourceSetRestriction=*/null))
            U
          }
        )).toString

      return c.page.render(parsed).code


  override def buildPageContent(context: FlowContent, page: ContentPage): Unit = 
    page match
      case s: StaticPageNode if !s.hasFrame() =>
      case _ =>  buildNavigation(context, page)

    page.getContent match
      case prc: PartiallyRenderedContent =>
        withHtml(context, render(prc, page))
      case content =>
        build(content, context, page, /*sourceSetRestriction=*/null)
  

  override def buildHtml(page: PageNode, resources: JList[String], kotlinxContent: FlowContentConsumer): String =
    val (pageTitle, pageResources, fromTemplate) = page match
      case static: StaticPageNode =>
        val res = if static.hasFrame() then resources else static.getEmbeddedResources()
        val title = static.loadedTemplate.templateFile.title()
        (title, res, !static.hasFrame())
      case _ =>
        (page.getName, resources, false)
    html(
      head(
        meta(charset := "utf-8"),
        meta(name := "viewport", content := "width=device-width, initial-scale=1"),
        title(pageTitle),
        linkResources(page, pageResources.asScala).toSeq,
        script(raw(s"""var pathToRoot = "${getLocationProvider.pathToRoot(page)}";"""))
      ),
      body(
        if fromTemplate then
          raw(buildWithKotlinx(kotlinxContent))
        else
          div(id := "container")(
            div(id := "leftColumn")(
              div(id := "logo"),
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
                span(cls := "pull-right")(
                  raw("Generated by&nbsp;"),
                  a(href := "https://github.com/lampepfl/scala3doc")("Scala3doc")
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

  private def linkResources(page: PageNode, resources: Iterable[String]): Iterable[AppliedTag] =
    def fileExtension(url: String): String =
      val param = url.indexOf('?')
      val end = if param < 0 then url.length else param
      val point = url.lastIndexOf('.', end)
      url.substring(point+1, end)

    def resolveLink(url: String): String =
      if URI(url).isAbsolute then url else resolveRoot(page, url)

    for res <- resources yield
      fileExtension(res) match
        case "css" => link(rel := "stylesheet", href := resolveLink(res))
        case "js" => script(`type` := "text/javascript", src := resolveLink(res), defer := "true")
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
