package dotty.dokka

import org.jetbrains.dokka.plugability.DokkaContext
import org.jetbrains.dokka.pages._
import org.jetbrains.dokka.model._
import org.jetbrains.dokka._
import scalatags.Text.all._
import scalatags.Text.tags2.{title, main, nav}
import scalatags.Text.TypedTag
import collection.JavaConverters._
import com.virtuslab.dokka.site.SiteRenderer
import com.virtuslab.dokka.site.BaseStaticSiteProcessor
import java.net.URI
import java.util.{List => JList, Set => JSet}
import kotlinx.html.FlowContent
import kotlinx.html.stream.StreamKt
import kotlinx.html.Gen_consumer_tagsKt

class ScalaHtmlRenderer(ctx: DokkaContext) extends SiteRenderer(ctx) {

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
            case other => super.buildContentNode(f, node, pageContext, sourceSetRestriciton)
        }
    }

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

    override def buildCodeBlock(
        f: FlowContent,
        code: ContentCodeBlock,
        pageContext: ContentPage,
    ): Unit = {
        // we cannot use Scalatags, because we need to call buildContentNode
        import kotlinx.html.{Gen_consumer_tagsKt => dsl}
        val c = f.getConsumer
        val U = kotlin.Unit.INSTANCE

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

    override def buildHtml(page: PageNode, resources: JList[String], kotlinxContent: FlowContentConsumer): String =
        val (pageTitle, pageResources, fromTemplate) = page match
            case static: BaseStaticSiteProcessor.StaticPageNode =>
                val res = if static.hasFrame then resources else static.resources
                val title = static.getLoadedTemplate.getTemplateFile.title
                (title, res, !static.hasFrame)
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
                    div(
                        id := "container",
                        div(
                            id := "leftColumn",
                            div(id := "logo"),
                            div(id := "paneSearch"),
                            nav(id := "sideMenu"),
                        ),
                        div(
                            id := "main",
                            div (
                                id := "leftToggler",
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

    private def linkResources(page: PageNode, resources: Iterable[String]): Iterable[Frag] =
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
                case "js" => script(`type` := "text/javascript", src := resolveLink(res), defer)
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
