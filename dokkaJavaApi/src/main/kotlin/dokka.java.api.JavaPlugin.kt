package dokka.java.api

import org.jetbrains.dokka.CoreExtensions
import org.jetbrains.dokka.DokkaConfiguration
import org.jetbrains.dokka.base.DokkaBase
import org.jetbrains.dokka.model.DModule
import org.jetbrains.dokka.plugability.DokkaContext
import org.jetbrains.dokka.plugability.DokkaPlugin
import org.jetbrains.dokka.transformers.documentation.PreMergeDocumentableTransformer
import org.jetbrains.dokka.transformers.sources.SourceToDocumentableTranslator

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

    abstract fun createSourceToDocumentableTranslator(cxt: DokkaContext, sourceSet: SourceSetWrapper): DModule
}
