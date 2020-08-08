package dokka.java.api.com.virtuslab.dokka.site

import org.jetbrains.dokka.base.resolvers.local.DefaultLocationProvider
import org.jetbrains.dokka.base.resolvers.local.LocationProvider
import org.jetbrains.dokka.base.resolvers.local.LocationProviderFactory
import org.jetbrains.dokka.pages.ContentPage
import org.jetbrains.dokka.pages.PageNode
import org.jetbrains.dokka.pages.RootPageNode
import org.jetbrains.dokka.plugability.DokkaContext

class StaticSiteLocationProviderFactory(val ctx: DokkaContext) : LocationProviderFactory {
    override fun getLocationProvider(pageNode: RootPageNode): LocationProvider {
        return StaticSiteLocationProvider(ctx, pageNode)
    }
}

class StaticSiteLocationProvider(ctx: DokkaContext, pageNode: RootPageNode): DefaultLocationProvider(pageNode, ctx) {
    override fun pathTo(node: PageNode, context: PageNode?): String =
        if (node is BaseStaticSiteProcessor.DocPageNode && node.dri.contains(docsRootDRI))
            "index"
        else
            super.pathTo(node, context)
}