package dokka.java.api

import org.jetbrains.dokka.CoreExtensions
import org.jetbrains.dokka.DokkaConfiguration
import org.jetbrains.dokka.base.DokkaBase
import org.jetbrains.dokka.base.signatures.KotlinSignatureProvider
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
import org.jetbrains.dokka.transformers.pages.PageTransformer
import org.jetbrains.dokka.transformers.sources.SourceToDocumentableTranslator
import org.jetbrains.dokka.utilities.DokkaLogger
import java.util.function.Consumer

data class SourceSetWrapper(val sourceSet: DokkaConfiguration.DokkaSourceSet) {
    fun toSet(): Set<DokkaConfiguration.DokkaSourceSet> = setOf(sourceSet)
    fun <T> asMap(value: T): Map<DokkaConfiguration.DokkaSourceSet, T> = mapOf(sourceSet to value)
}

abstract class JavaDokkaPlugin : DokkaPlugin() {
    private val dokkaBase by lazy { plugin<DokkaBase>() }

    val provideDottyDocs by extending {
        CoreExtensions.sourceToDocumentableTranslator providing { ctx ->
            object : SourceToDocumentableTranslator {
                override fun invoke(sourceSet: DokkaConfiguration.DokkaSourceSet, context: DokkaContext): DModule =
                    createSourceToDocumentableTranslator(context, SourceSetWrapper(sourceSet))
            }
        } override dokkaBase.descriptorToDocumentableTranslator
    }

    val scalaSignatureProvider by extending {
        dokkaBase.signatureProvider providing { ctx ->
            createSignatureProvider(ctx.single(dokkaBase.commentsToContentConverter), ctx.logger)
        } override dokkaBase.kotlinSignatureProvider
    }

    val scalaLogoProvider by extending {
        dokkaBase.htmlPreprocessors providing { ctx ->
            createLogoInstaller(ctx)
        } order { after(dokkaBase.resourceInstaller) }
    }

    abstract fun createSourceToDocumentableTranslator(cxt: DokkaContext, sourceSet: SourceSetWrapper): DModule
    abstract fun createSignatureProvider(ctcc: CommentsToContentConverter, logger: DokkaLogger): SignatureProvider
    abstract fun createLogoInstaller(ctx: DokkaContext) : PageTransformer
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