package dotty.dokka

import org.jetbrains.dokka.plugability.DokkaContext
import org.jetbrains.dokka.pages._
import org.jetbrains.dokka.model._
import org.jetbrains.dokka.base.renderers.html.HtmlRenderer
import org.jetbrains.dokka.base.renderers.DefaultRenderer
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
import org.jetbrains.dokka.base.resolvers.local.LocationProvider
import org.jetbrains.dokka.base.transformers.pages.comments.DocTagToContentConverter
import scala.util.Try
import org.jetbrains.dokka.base.renderers.html.SearchbarDataInstaller
import org.jsoup.Jsoup
import java.nio.file.Paths

trait SignatureRenderer:
  def currentDri: DRI
  def link(dri: DRI): Option[String]

  def renderElement(e: String | (String, DRI) | Link) = renderElementWith(e)

  def renderLink(name: String, dri: DRI, modifiers: AppliedAttr*) =
    link(dri) match
      case Some(link) => a(href := link, modifiers)(name)
      case _ => span(Attr("data-unresolved-link") := "", modifiers)(name)

  def renderElementWith(e: String | (String, DRI) | Link, modifiers: AppliedAttr*) = e match
    case (name, dri) => renderLink(name, dri, modifiers:_*)
    case name: String => raw(name)
    case Link(name, dri) => renderLink(name, dri, modifiers:_*)

class SignatureRendererImpl(pageContext: ContentPage, sourceSetRestriciton: JSet[DisplaySourceSet], locationProvider: LocationProvider) extends SignatureRenderer:
  val currentDri = pageContext.getDri.asScala.head.asScala

  def link(dri: DRI): Option[String] =
    Option(locationProvider.resolve(dri.asDokka, sourceSetRestriciton, pageContext))

class DokkaScalaHtmlRenderer(using ctx: DokkaContext) extends HtmlRenderer(ctx) {
  val args = summon[DocContext].args

  def init(ourRenderer: dotty.dokka.renderers.HtmlRenderer): Unit =
    val f = classOf[DefaultRenderer[_]].getDeclaredField("locationProvider")
    f.setAccessible(true)
    f.set(this, new LocationProvider {
      type DSS = JSet[org.jetbrains.dokka.model.DisplaySourceSet]
      def resolve(to: DDRI, sourceSets: DSS, context: PageNode): String =
        context match
          case null => ???
          case fromPage: ContentPage =>
            val from = fromPage.getDri.asScala.head
            val path = ourRenderer.pathToPage(to.asScala, from.asScala)
            path

      // None other operation is needed
      // This will goes away once we migrate away from dokka
      def resolve(node: PageNode, context: PageNode, skipExtension: Boolean): String = ???
      def pathToRoot(from: PageNode): String = ???
      def ancestors(node: PageNode): JList[PageNode] = ???
      def expectedLocationForDri(dri: DDRI): String = ???
    })

  override def render(root: RootPageNode): Unit = ???
    // We are using dokka renderer to render doc strings

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
      case n: HtmlContentNode =>
        withHtml(f, raw(n.body).toString)
      case mi: MemberInfo =>
        val memberHtml = div(renderers(pageContext)._2.fullMember(mi.member))
        withHtml(f, memberHtml.toString)
      case other => super.buildContentNode(f, node, pageContext, sourceSetRestriciton)
    }
  }

  private def renderers(pageContext: ContentPage): (SignatureRenderer, MemberRenderer) =
    val renderer = SignatureRendererImpl(pageContext, sourceSets, getLocationProvider)
    (renderer, new MemberRenderer(renderer, buildWithKotlinx(_, pageContext, null)))

  private def buildNavigation(r: SignatureRenderer)(rootNav: NavigationNode): AppliedTag =
    val currentPageDri = r.currentDri

    def renderNested(nav: NavigationNode): (Boolean, AppliedTag) =
      val isSelected = nav.dri == currentPageDri
      def linkHtml(exapnded: Boolean = false) =
        val attrs = if (isSelected) Seq(cls := "selected expanded") else Nil
        a(href := r.link(nav.dri).getOrElse("#"), attrs)(nav.name)

      nav.nested match
        case Nil => isSelected -> div(linkHtml())
        case children =>
          val nested = children.map(renderNested)
          val expanded = nested.exists(_._1) | nav == rootNav
          val attr = if expanded || isSelected then Seq(cls := "expanded") else Nil
          (isSelected || expanded) -> div(attr)(
            linkHtml(expanded),
            span(),
            nested.map(_._2)
          )

    renderNested(rootNav)._2

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

  private val HashRegex = "([^#]+)(#.+)".r

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

  def buildWithKotlinx(node: ContentNode, pageContext: ContentPage, sourceSetRestriction: JSet[DisplaySourceSet]): String =
    Gen_consumer_tagsKt.div(
      StreamKt.createHTML(true, false),
      null,
      (div) => {build(node, div, pageContext, sourceSetRestriction); kotlin.Unit.INSTANCE}
    ).toString.stripPrefix("<div>").stripSuffix("</div>\n")

  def buildWithKotlinx(func: FlowContentConsumer): String =
    Gen_consumer_tagsKt.div(
      StreamKt.createHTML(true, false),
      null,
      func
    ).toString.stripPrefix("<div>").stripSuffix("</div>\n")
}
