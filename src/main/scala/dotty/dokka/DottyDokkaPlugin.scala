package dotty.dokka

import org.jetbrains.dokka.plugability._
import org.jetbrains.dokka.transformers.sources._

import org.jetbrains.dokka.DokkaConfiguration
import org.jetbrains.dokka.model._
import org.jetbrains.dokka.links._
import org.jetbrains.dokka.model.doc._
import org.jetbrains.dokka.base.parsers._
import org.jetbrains.dokka.plugability.DokkaContext
import dokka.java.api._
import collection.JavaConverters._
import org.jetbrains.dokka.model.properties.PropertyContainer
import dotty.dokka.tasty.DokkaTastyInspector
import org.jetbrains.dokka.base.transformers.pages.comments.CommentsToContentConverter
import org.jetbrains.dokka.utilities.DokkaLogger

class DottyDokkaPlugin extends JavaDokkaPlugin:
  override def createSourceToDocumentableTranslator(cxt: DokkaContext, sourceSet: SourceSetWrapper): DModule = cxt.getConfiguration match {
    case dottyConfig: DottyDokkaConfig =>
      val inspector = DokkaTastyInspector(sourceSet, new MarkdownParser(null, null, cxt.getLogger), dottyConfig)
      inspector.inspect(dottyConfig.docConfiguration.classpath, dottyConfig.docConfiguration.tastyFiles)
    
      new DModule(
        sourceSet.getSourceSet.getSourceSetID.getModuleName,
        inspector.result().asJava,
        Map().asJava,
        null,
        sourceSet.toSet,
        PropertyContainer.Companion.empty()
      )
    case _ =>
      ???
  }

  override def createSignatureProvider(ctcc: CommentsToContentConverter, logger: DokkaLogger) = new ScalaSignatureProvider(ctcc, logger) 