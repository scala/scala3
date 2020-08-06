package dokka.java.api.com.virtuslab.dokka.site

import org.jetbrains.dokka.CoreExtensions
import org.jetbrains.dokka.base.DokkaBase
import org.jetbrains.dokka.plugability.DokkaPlugin

class StaticSitePlugin: DokkaPlugin() {
    private val dokkaBase by lazy { plugin<DokkaBase>() }

    val customDocumentationProvider by extending {
        dokkaBase.htmlPreprocessors providing { ctx ->
            SitePagesCreator(ctx)
        } order {
            after(dokkaBase.navigationPageInstaller)
            before(dokkaBase.styleAndScriptsAppender)
        }
    }

    val customDocumentationResources by extending {
        dokkaBase.htmlPreprocessors providing { ctx ->
            SiteResourceManager(ctx)
        } order {
            // We want our css and scripts after default ones
            after(dokkaBase.styleAndScriptsAppender)
        }
    }

    val dokkaJavadocPlugin by extending {
        (CoreExtensions.renderer
                providing { ctx -> ExternalDocsToolRenderer(ctx) }
                override dokkaBase.htmlRenderer)
    }
}