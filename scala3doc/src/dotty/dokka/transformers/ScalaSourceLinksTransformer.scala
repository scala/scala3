package dotty.dokka

import org.jetbrains.dokka.transformers.documentation.DocumentableTransformer
import org.jetbrains.dokka.pages._
import collection.JavaConverters
import collection.JavaConverters._
import org.jetbrains.dokka.plugability.DokkaContext
import org.jetbrains.dokka.model._
import org.jetbrains.dokka.{DokkaConfiguration$DokkaSourceSet, DokkaConfiguration$SourceLinkDefinition}
import org.jetbrains.dokka.DokkaConfiguration
import org.jetbrains.dokka.utilities.DokkaLogger
import org.jetbrains.dokka.base.signatures.SignatureProvider
import org.jetbrains.dokka.base.transformers.pages.comments.CommentsToContentConverter
import org.jetbrains.dokka.model.properties._
import dotty.dokka.model.api._

class ScalaSourceLinksTransformer(
    val ctx: DokkaContext,
    val commentsToContentConverter: CommentsToContentConverter,
    val signatureProvider: SignatureProvider,
    val logger: DokkaLogger
) extends DocumentableTransformer:

  val sourceLinks = ctx.getConfiguration.getSourceSets.asScala.flatMap(s => s.getSourceLinks.asScala.map(l => SourceLink(l, s)))
  val pageBuilder = ScalaPageContentBuilder(commentsToContentConverter, signatureProvider, logger)

  case class SourceLink(val path: String, val url: String, val lineSuffix: Option[String], val sourceSetData: DokkaSourceSet)

  object SourceLink {
    def apply(sourceLinkDef: DokkaConfiguration$SourceLinkDefinition, sourceSetData: DokkaSourceSet): SourceLink =
      SourceLink(sourceLinkDef.getLocalDirectory, sourceLinkDef.getRemoteUrl.toString, Option(sourceLinkDef.getRemoteLineSuffix), sourceSetData)
  }


  override def invoke(input: DModule, context: DokkaContext): DModule =
    input.updateMembers {
      case c0: (Member & WithSources & WithExtraProperties[_]) =>
        val c = c0.asInstanceOf[Member & WithSources & WithExtraProperties[Member]]
        c.withNewExtras(c.getExtra plus getSourceLinks(c))
      case c => c
    }


  private def getSourceLinks(doc: WithSources): ExtraProperty[Member] = {
    val urls = doc.getSources.asScala.toMap.flatMap{
      case (key,value) => sourceLinks.find(s => value.getPath.contains(s.path) && key == s.sourceSetData).map(
          link => (key, createLink(value, link))
        )
    }.collect{
      case (key, Some(value)) => (key,value)
    }.toMap

    SourceLinks(urls)
  }

  private def createLink(source: DocumentableSource, link: SourceLink): Option[String] = source match {
    case s: TastyDocumentableSource => Some(s.lineNumber).map( line =>
      link.url + s.path.split(link.path)(1) + link.lineSuffix.map(_ + (line + 1)).getOrElse("") //TASTY enumerates lines from 0
    )
    case other => None
  }
