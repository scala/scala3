package dokka.java.api

import org.jetbrains.dokka.CoreExtensions
import org.jetbrains.dokka.DokkaConfiguration
import org.jetbrains.dokka.base.DokkaBase
import org.jetbrains.dokka.base.signatures.SignatureProvider
import org.jetbrains.dokka.base.transformers.pages.comments.CommentsToContentConverter
import org.jetbrains.dokka.base.translators.documentables.PageContentBuilder
import org.jetbrains.dokka.model.DModule
import org.jetbrains.dokka.model.Documentable
import org.jetbrains.dokka.pages.ContentGroup
import org.jetbrains.dokka.pages.ContentKind
import org.jetbrains.dokka.pages.Kind
import org.jetbrains.dokka.pages.Style
import org.jetbrains.dokka.plugability.DokkaContext
import org.jetbrains.dokka.plugability.DokkaPlugin
import org.jetbrains.dokka.renderers.Renderer
import org.jetbrains.dokka.transformers.documentation.DocumentableToPageTranslator
import org.jetbrains.dokka.transformers.documentation.DocumentableTransformer
import org.jetbrains.dokka.transformers.pages.PageTransformer
import org.jetbrains.dokka.transformers.sources.SourceToDocumentableTranslator
import org.jetbrains.dokka.utilities.DokkaLogger
import java.util.function.Consumer
import com.virtuslab.dokka.site.StaticSitePlugin

data class SourceSetWrapper(val sourceSet: DokkaConfiguration.DokkaSourceSet) {
    fun toSet(): Set<DokkaConfiguration.DokkaSourceSet> = setOf(sourceSet)
    fun <T> asMap(value: T): Map<DokkaConfiguration.DokkaSourceSet, T> = mapOf(sourceSet to value)
}

abstract class JavaDokkaPlugin : DokkaPlugin() {
    private val dokkaBase by lazy { plugin<DokkaBase>() }
    private val dokkaSite by lazy { plugin<StaticSitePlugin>() }

    val outputWriter by extensionPoint<OutputWriter>()

    val provideDottyDocs by extending {
        CoreExtensions.sourceToDocumentableTranslator providing { _ ->
            object : SourceToDocumentableTranslator {
                override fun invoke(sourceSet: DokkaConfiguration.DokkaSourceSet, context: DokkaContext): DModule =
                    createSourceToDocumentableTranslator(context, SourceSetWrapper(sourceSet))
            }
        }  override dokkaBase.psiToDocumentableTranslator
    }

    // Just turn off another translator since multiple overrides does not work
    val disableOtherTranslator by extending {
        CoreExtensions.sourceToDocumentableTranslator providing { _ ->
            object : SourceToDocumentableTranslator {
                override fun invoke(sourceSet: DokkaConfiguration.DokkaSourceSet, context: DokkaContext): DModule =
                    TODO()
            }
        }  override dokkaBase.descriptorToDocumentableTranslator applyIf{ false }
    }
    
    val scalaSignatureProvider by extending {
        dokkaBase.signatureProvider providing { ctx ->
            createSignatureProvider(ctx.single(dokkaBase.commentsToContentConverter), ctx.logger)
        } override dokkaBase.kotlinSignatureProvider
    }

    val scalaResourceInstaller by extending {
        dokkaBase.htmlPreprocessors providing { ctx ->
            createResourceInstaller(ctx)
        } order { after(dokkaBase.resourceInstaller) }
    }

    val scalaEmbeddedResourceAppender by extending {
        dokkaBase.htmlPreprocessors providing {ctx ->
            createEmbeddedResourceAppender(ctx)
        } order { after(dokkaBase.styleAndScriptsAppender) }
    }

    val scalaDocumentableToPageTranslator by extending {
        CoreExtensions.documentableToPageTranslator providing { ctx ->
            createDocumentableToPageTranslator(
                ctx.single(dokkaBase.commentsToContentConverter),
                ctx.single(dokkaBase.signatureProvider),
                ctx.logger
            )
        } override dokkaBase.documentableToPageTranslator
    }

    val packageHierarchyTransformer by extending {
        CoreExtensions.pageTransformer providing { ctx ->
            createPackageHierarchyTransformer(ctx)
        } order { before(dokkaBase.rootCreator) }
    }

    val inheritanceInformationTransformer by extending {
        CoreExtensions.documentableTransformer providing { ctx ->
            createInheritanceInformationTransformer(ctx)
        }
    }

    val sourceLinksTransformer by extending {
        CoreExtensions.documentableTransformer providing { ctx ->
            createSourceLinksTransformer(
                ctx,
                ctx.single(dokkaBase.commentsToContentConverter),
                ctx.single(dokkaBase.signatureProvider),
                ctx.logger
            )
        }
    }

    val htmlRenderer by extending {
        CoreExtensions.renderer providing { ctx ->
            createHtmlRenderer(ctx)
        } override dokkaSite.customRenderer
    }

    val muteDefaultSourceLinksTransformer by extending {
        CoreExtensions.pageTransformer providing { ctx ->
            object : PageTransformer {
                override fun invoke(input: org.jetbrains.dokka.pages.RootPageNode) = input
            }
        } override dokkaBase.sourceLinksTransformer
    }


    abstract fun createSourceToDocumentableTranslator(cxt: DokkaContext, sourceSet: SourceSetWrapper): DModule
    abstract fun createSignatureProvider(ctcc: CommentsToContentConverter, logger: DokkaLogger): SignatureProvider
    abstract fun createResourceInstaller(ctx: DokkaContext) : PageTransformer
    abstract fun createEmbeddedResourceAppender(ctx: DokkaContext) : PageTransformer
    abstract fun createDocumentableToPageTranslator(
        commentsToContentConverter: CommentsToContentConverter,
        signatureProvider: SignatureProvider,
        logger: DokkaLogger
    ) : DocumentableToPageTranslator
    abstract fun createPackageHierarchyTransformer(ctx: DokkaContext) : PageTransformer
    abstract fun createInheritanceInformationTransformer(ctx: DokkaContext): DocumentableTransformer
    abstract fun createSourceLinksTransformer(
            ctx: DokkaContext,
            commentsToContentConverter: CommentsToContentConverter,
            signatureProvider: SignatureProvider,
            logger: DokkaLogger
    ): DocumentableTransformer

    abstract fun createHtmlRenderer(ctx: DokkaContext): Renderer
}

// TODO we probably does not need that
class JPageContentBuilder(cc: CommentsToContentConverter, sp: SignatureProvider, l: DokkaLogger) :
    PageContentBuilder(cc, sp, l) {
    fun mkContent(
        d: Documentable,
        kind: Kind = ContentKind.Main,
        styles: Set<Style>,
        op: Consumer<DocumentableContentBuilder>
    ): ContentGroup =
        contentFor(d.dri, d.sourceSets, kind, styles) {
            op.accept(this)
        }
}