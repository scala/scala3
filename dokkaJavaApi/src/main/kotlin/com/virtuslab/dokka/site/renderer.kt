package dokka.java.api.com.virtuslab.dokka.site

import kotlinx.html.FlowContent
import kotlinx.html.div
import kotlinx.html.unsafe
import org.jetbrains.dokka.pages.ContentPage
import org.jetbrains.dokka.plugability.DokkaContext

class ExternalDocsToolRenderer(context: DokkaContext) : org.jetbrains.dokka.base.renderers.html.HtmlRenderer(context) {
    override fun buildPageContent(context: FlowContent, page: ContentPage) {
        context.buildNavigation(page)
        fun FlowContent.render(txt: String) = div { unsafe { +txt } }
        when (page) {
            is SelfRendered -> context.render(page.htmlText())
            else -> page.content.build(context, page)
        }
    }
}
