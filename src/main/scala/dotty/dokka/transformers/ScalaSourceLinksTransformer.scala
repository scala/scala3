package dotty.dokka

import org.jetbrains.dokka.transformers.pages.{PageTransformer}
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

class ScalaSourceLinksTransformer(
      val ctx: DokkaContext,        
      val commentsToContentConverter: CommentsToContentConverter,
      val signatureProvider: SignatureProvider,
      val logger: DokkaLogger
) extends PageTransformer:

    val sourceLinks = ctx.getConfiguration.getSourceSets.asScala.flatMap(s => s.getSourceLinks.asScala.map(l => SourceLink(l, s)))
    val pageBuilder = ScalaPageContentBuilder(commentsToContentConverter, signatureProvider, logger)

    case class SourceLink(val path: String, val url: String, val lineSuffix: Option[String], val sourceSetData: DokkaConfiguration.DokkaSourceSet)

    object SourceLink{
        def apply(sourceLinkDef: DokkaConfiguration$SourceLinkDefinition, sourceSetData: DokkaConfiguration.DokkaSourceSet): SourceLink = 
            SourceLink(sourceLinkDef.getPath, sourceLinkDef.getUrl, Option(sourceLinkDef.getLineSuffix), sourceSetData)
    }

    override def invoke(input: RootPageNode): RootPageNode = input.transformContentPagesTree(page => page.getDocumentable match{
        case d: WithExpectActual => appendSourceLink(page, d)
        case other => page
    })


    private def appendSourceLink(page: ContentPage, doc: WithExpectActual): ContentPage = {
        val urls = doc.getSources.asScala.toMap.flatMap{
            case (key,value) => sourceLinks.find(s => value.getPath.contains(s.path) && key == s.sourceSetData).map(
                    link => (key, createLink(value, link))
                )
        }.collect{
            case (key, Some(value)) => (key,value)
        }.toMap

        if(!urls.isEmpty) appendContent(page, urls) else page
    }

    private def createLink(source: DocumentableSource, link: SourceLink): Option[String] = source match {
        case s: TastyDocumentableSource => Some(s.lineNumber).map( line => 
            link.url + s.path.split(link.path)(1) + link.lineSuffix.map(_ + (line + 1)).getOrElse("") //TASTY enumerates lines from 0
        )
        case other => None
    }

    private def appendContent(page: ContentPage, urls: Map[DokkaConfiguration$DokkaSourceSet, String]): ContentPage = {
        def contentFunction: ScalaPageContentBuilder#ScalaDocumentableContentBuilder => ScalaPageContentBuilder#ScalaDocumentableContentBuilder = bdr => bdr
            .header(2, "Sources", kind = ContentKind.Source)()
            .table(
                kind = ContentKind.Source, 
                styles = Set(), 
                extra = bdr.mainExtra plus SimpleAttr.Companion.header("Sources")
            ){ tbdr => urls.foldLeft[ScalaPageContentBuilder#ScalaTableBuilder](tbdr){ 
                    case (tablebdr, (sourceSet, url)) => tablebdr
                        .cell(sourceSets = Set(sourceSet)){ cbdr => cbdr
                            .resolvedLink("(source)", url)
                        }
                }
                
            }
        page.getContent match {
            case g: ContentGroup => 
                val content = getContentGroupWithParents(g, p => p.getStyle.asScala.contains(ContentStyle.TabbedContent))
                if(content.size > 0) {
                    val addedContent = pageBuilder.contentForDRI(page.getDri.asScala.head, page.getDocumentable.getSourceSets.asScala.toSet, buildBlock = contentFunction)

                    val modifiedContent = content(0).copy(
                        (content(0).getChildren.asScala ++ List(addedContent)).asJava,
                        content(0).getDci,
                        content(0).getSourceSets,
                        content(0).getStyle,
                        content(0).getExtra
                    )
                    page.modified(
                        page.getName,
                        modifyContentGroup(content, modifiedContent),
                        page.getDri,
                        page.getEmbeddedResources,
                        page.getChildren
                    )
                } else page
                
            case other => throw IllegalArgumentException("Content of page is not ContentGroup")
        }

    }