package dokka.java.api.com.virtuslab.dokka.site

import org.jetbrains.dokka.base.renderers.html.NavigationNode
import org.jetbrains.dokka.base.renderers.html.NavigationPage
import org.jetbrains.dokka.links.DRI
import org.jetbrains.dokka.model.Documentable
import org.jetbrains.dokka.pages.*
import org.jetbrains.dokka.plugability.DokkaContext
import org.jetbrains.dokka.transformers.pages.PageTransformer
import java.io.File

const val ExternalDocsTooKey = "ExternalDocsTooKey"

interface SelfRendered {
    fun htmlText(): String
}

abstract class BaseStaticSiteProcessor(cxt: DokkaContext) : PageTransformer {

    final override fun invoke(input: RootPageNode): RootPageNode =
        rawRoot?.let { transform(input) } ?: input

    protected abstract fun transform(input: RootPageNode): RootPageNode

    private val rawRoot: File? = cxt.configuration.pluginsConfiguration.get(ExternalDocsTooKey)?.let { File(it) }
    val root = rawRoot ?: File("unknown")

    protected val mySoruceSet = cxt.configuration.sourceSets.toSet()
    protected val docsFile = File(root, "docs")

    protected fun File.asDri(): DRI {
        val relativePath = root.toPath().relativize(toPath()).toString().replace(File.separatorChar, '.')
        return DRI("_.$relativePath")
    }

    inner class DocPageNode(
        private val from: File,
        private val template: TemplateFile?,
        override val children: List<DocPageNode>,
        val resolved: ResolvedPage,
        override val dri: Set<DRI>,
        override val embeddedResources: List<String> = emptyList()
    ) : ContentPage, SelfRendered {
        override val name: String = template?.name() ?: from.name

        fun title() = template?.title() ?: name

        override val documentable: Documentable? = null
        override val content: ContentNode = ContentText("_", DCI(dri, ContentKind.Empty), mySoruceSet)

        override fun htmlText(): String = resolved.html

        override fun modified(
            name: String,
            content: ContentNode,
            dri: Set<DRI>,
            embeddedResources: List<String>,
            children: List<PageNode>
        ): ContentPage =
            DocPageNode(from, template, children.filterIsInstance<DocPageNode>(), resolved, dri, embeddedResources)

        override fun modified(name: String, children: List<PageNode>): PageNode =
            DocPageNode(from, template, children.filterIsInstance<DocPageNode>(), resolved, dri, embeddedResources)
    }
}

class SiteResourceManager(cxt: DokkaContext) : BaseStaticSiteProcessor(cxt) {
    private fun listResources(nodes: List<PageNode>): Set<String> =
        nodes.flatMap {
            when {
                it is DocPageNode ->
                    listResources(it.children) + it.resolved.resources
                else -> emptySet()
            }
        }.toSet()


    override fun transform(input: RootPageNode): RootPageNode {
        val resources = listResources(input.children)
        val resourcePages = resources.map { path ->
            RendererSpecificResourcePage(path, emptyList(), RenderingStrategy.Write(File(root, path).readText()))
        }
        val modified = input.transformContentPagesTree {
            when (it) {
                is DocPageNode -> it.modified(embeddedResources = it.embeddedResources + it.resolved.resources)
                else -> it
            }
        }
        return modified.modified(children = resourcePages + modified.children)
    }
}


class SitePagesCreator(cxt: DokkaContext) : BaseStaticSiteProcessor(cxt) {

    override fun transform(input: RootPageNode): RootPageNode {
        val (navigationPage, rest) = input.children.partition { it is NavigationPage }
        val defaultNavigation = (navigationPage.single() as NavigationPage).root

        val children = loadFiles()

        fun toNavigationNode(c: DocPageNode): NavigationNode =
            NavigationNode(
                c.title(),
                c.dri.first(),
                mySoruceSet,
                c.children.map { toNavigationNode(it) }
            )

        val mergedRoots = NavigationNode(
            defaultNavigation.name,
            DRI(extra = "API"),
            defaultNavigation.sourceSets,
            children.map { toNavigationNode(it) } + listOf(
                NavigationNode(
                    "API",
                    defaultNavigation.dri,
                    defaultNavigation.sourceSets,
                    defaultNavigation.children
                )
            )
        )
        return input.modified(children = rest + listOf(NavigationPage(mergedRoots)) + children)
    }

    private val layouts: Map<String, TemplateFile> by lazy {
        val layoutRoot = File(root, "_layouts")
        val dirs: Array<File> =  layoutRoot.listFiles() ?: emptyArray()
        dirs.map { loadTemplateFile(it) }.map { it.name() to it }.toMap()
    }

    private fun renderDocs(from: File): DocPageNode? = if (from.name.startsWith("_")) null else try {
        val dri = setOf(from.asDri())

        val children =  from.listFiles()?.mapNotNull { renderDocs(it) } ?: emptyList()
        val templateFile = if (from.isDirectory) null else loadTemplateFile(from)
        val content = try {
            val context = RenderingContext(emptyMap(), layouts)
            if (from.isDirectory) ResolvedPage("TODO add content", emptyList()) else templateFile!!.resolve(context)
        } catch (e: Throwable) {
            val msg = "Error rendering $from: ${e.message}"
            println("ERROR: $msg") // TODO proper error handling
            ResolvedPage(msg, emptyList())
        }

        DocPageNode(from, templateFile, children, content, dri)
    } catch (e: RuntimeException) {
        e.printStackTrace()
        null
    }

    private fun loadFiles(): List<DocPageNode> =
        docsFile.listFiles()?.mapNotNull { renderDocs(it) } ?: emptyList()

}