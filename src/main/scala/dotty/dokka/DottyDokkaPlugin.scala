package dotty.dokka

import org.jetbrains.dokka.plugability._
import org.jetbrains.dokka.transformers.sources._

import org.jetbrains.dokka.DokkaConfiguration
import org.jetbrains.dokka.model.DModule
import org.jetbrains.dokka.plugability.DokkaContext

class ProvideModule extends SourceToDocumentableTranslator:
  def invoke(sourceSet: DokkaConfiguration.DokkaSourceSet, context: DokkaContext): DModule =
    ???

class DottyDokkaPlugin extends DokkaPlugin:
  extending