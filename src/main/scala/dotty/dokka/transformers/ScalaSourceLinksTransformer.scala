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

class ScalaSourceLinksTransformer(
      val ctx: DokkaContext,        
      val commentsToContentConverter: CommentsToContentConverter,
      val signatureProvider: SignatureProvider,
      val logger: DokkaLogger
) extends DocumentableTransformer:

    val sourceLinks = ctx.getConfiguration.getSourceSets.asScala.flatMap(s => s.getSourceLinks.asScala.map(l => SourceLink(l, s)))
    val pageBuilder = ScalaPageContentBuilder(commentsToContentConverter, signatureProvider, logger)

    case class SourceLink(val path: String, val url: String, val lineSuffix: Option[String], val sourceSetData: DokkaConfiguration.DokkaSourceSet)

    object SourceLink {
        def apply(sourceLinkDef: DokkaConfiguration$SourceLinkDefinition, sourceSetData: DokkaConfiguration.DokkaSourceSet): SourceLink = 
            SourceLink(sourceLinkDef.getLocalDirectory, sourceLinkDef.getRemoteUrl.toString, Option(sourceLinkDef.getRemoteLineSuffix), sourceSetData)
    }

    private def processDocumentable[T] (d: T): T = (d match {
        case m: DModule => m.copy(
            m.getName,
            m.getPackages.asScala.map(processDocumentable).asJava,
            m.getDocumentation,
            m.getExpectPresentInSet,
            m.getSourceSets,
            m.getExtra
        )
        case p: DPackage => p.copy(
            p.getDri,
            p.getFunctions.asScala.map(processDocumentable).asJava,
            p.getProperties.asScala.map(processDocumentable).asJava,
            p.getClasslikes.asScala.map(processDocumentable).asJava,
            p.getTypealiases.asScala.map(processDocumentable).asJava,
            p.getDocumentation,
            p.getExpectPresentInSet,
            p.getSourceSets,
            p.getExtra
        )
        case c: DClass => c.copy(
            c.getDri,
            c.getName,
            c.getConstructors.asScala.map(processDocumentable).asJava,
            c.getFunctions.asScala.map(processDocumentable).asJava,
            c.getProperties.asScala.map(processDocumentable).asJava,
            c.getClasslikes.asScala.map(processDocumentable).asJava,
            c.getSources,
            c.getVisibility,
            c.getCompanion,
            c.getGenerics,
            c.getSupertypes,
            c.getDocumentation,
            c.getExpectPresentInSet,
            c.getModifier,
            c.getSourceSets,
            c.getExtra
        ).withNewExtras(c.getExtra plus getSourceLinks(c))
        case f: DFunction => f.withNewExtras(f.getExtra plus getSourceLinks(f))
        case p: DProperty => p.withNewExtras(p.getExtra plus getSourceLinks(p))
        case other => other
    }).asInstanceOf[T]

    override def invoke(input: DModule, context: DokkaContext): DModule = processDocumentable(input)


    private def getSourceLinks(doc: WithExpectActual): SourceLinks = {
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