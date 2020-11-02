package dotty.dokka

import org.jetbrains.dokka.base.signatures.SignatureProvider
import org.jetbrains.dokka.base.transformers.pages.comments.CommentsToContentConverter
import org.jetbrains.dokka.model.DModule
import org.jetbrains.dokka.pages.ModulePageNode
import org.jetbrains.dokka.transformers.documentation.DocumentableToPageTranslator
import org.jetbrains.dokka.utilities.DokkaLogger

class ScalaDocumentableToPageTranslator(
  val commentsToContentConverter: CommentsToContentConverter,
  val signatureProvider: SignatureProvider,
  val logger: DokkaLogger
) extends DocumentableToPageTranslator {
  override def invoke(module: DModule): ModulePageNode = ScalaPageCreator(commentsToContentConverter, signatureProvider, logger).pageForModule(module)
}
