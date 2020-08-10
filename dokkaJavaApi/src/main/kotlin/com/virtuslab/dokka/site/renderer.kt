package dokka.java.api.com.virtuslab.dokka.site

import kotlinx.html.*
import kotlinx.html.stream.createHTML
import org.jetbrains.dokka.pages.ContentPage
import org.jetbrains.dokka.pages.PageNode
import org.jetbrains.dokka.plugability.DokkaContext
import java.net.URI

class ExternalDocsToolRenderer(context: DokkaContext) : org.jetbrains.dokka.base.renderers.html.HtmlRenderer(context) {
    override fun buildPageContent(context: FlowContent, page: ContentPage) {
        context.buildNavigation(page)
        fun FlowContent.render(txt: String) = div { unsafe { +txt } }
        val content = page.content
        when (content) {
            is PreRenderedContent -> context.render(content.html)
            else -> page.content.build(context, page)
        }
    }

    // TODO remove once html render has proper API
    private fun PageNode.root(path: String) = locationProvider.resolveRoot(this) + path
    private fun resolveLink(link: String, page: PageNode): String = if (URI(link).isAbsolute) link else page.root(link)

    // TODO change API of HTML renderer
    override fun buildHtml(page: PageNode, resources: List<String>, content: FlowContent.() -> Unit) =
        when (page) {
            is BaseStaticSiteProcessor.DocPageNode ->
                if (!page.dri.contains(docsRootDRI)) super.buildHtml(page, resources, content)
                else createHTML().html {
                    head {
                        meta(name = "viewport", content = "width=device-width, initial-scale=1", charset = "UTF-8")
                        title(page.name)
                        page.resolved.resources.forEach {
                            when {
                                it.substringBefore('?').substringAfterLast('.') == "css" -> link(
                                    rel = LinkRel.stylesheet,
                                    href = resolveLink(it, page)
                                )
                                it.substringBefore('?').substringAfterLast('.') == "js" -> script(
                                    type = ScriptType.textJavaScript,
                                    src = resolveLink(it, page)
                                ) {
                                    async = true
                                }
                                else -> unsafe { +it }
                            }
                        }
                        script { unsafe { +"""var pathToRoot = "${locationProvider.resolveRoot(page)}";""" } }
                    }
                    body {
                        unsafe { +page.resolved.html }
                    }
                }
            else ->
                super.buildHtml(page, resources, content)
        }

}
