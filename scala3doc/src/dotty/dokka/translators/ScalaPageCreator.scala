package dotty.dokka

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.chaining._
import org.jetbrains.dokka.base.translators.documentables.{DefaultPageCreator, PageContentBuilder}
import org.jetbrains.dokka.base.translators.documentables.PageContentBuilder$DocumentableContentBuilder
import org.jetbrains.dokka.base.signatures.SignatureProvider
import org.jetbrains.dokka.base.transformers.pages.comments.CommentsToContentConverter
import org.jetbrains.dokka.transformers.documentation.DocumentableToPageTranslator
import org.jetbrains.dokka.utilities.DokkaLogger
import org.jetbrains.dokka.model._
import org.jetbrains.dokka.pages._
import collection.JavaConverters._
import org.jetbrains.dokka.model.properties._
import org.jetbrains.dokka.base.transformers.documentables.CallableExtensions
import org.jetbrains.dokka.DokkaConfiguration$DokkaSourceSet
import org.jetbrains.dokka.base.resolvers.anchors._
import org.jetbrains.dokka.model.doc._
import dotty.dokka.model.api._
import dotty.dokka.model.api.Kind
import dotty.dokka.model.api.Link

type DocBuilder = ScalaPageContentBuilder#ScalaDocumentableContentBuilder

class ScalaPageCreator(
  commentsToContentConverter: CommentsToContentConverter,
  signatureProvider: SignatureProvider,
)(using ctx: DocContext)
  extends DefaultPageCreator(commentsToContentConverter, signatureProvider, ctx.logger):

  private val contentBuilder =
    ScalaPageContentBuilder(commentsToContentConverter, signatureProvider)

  override def pageForModule(m: DModule): ModulePageNode = super.pageForModule(m)

  private def pagesForMembers(member: Member): JList[PageNode] =
    val all = member
      .membersBy(_.kind.isInstanceOf[Classlike])
      .filter(m => m.origin == Origin.RegularlyDefined && m.inheritedFrom.isEmpty)
    all.map(pageForMember(_)).asJava

  override def pageForPackage(p: DPackage): PackagePageNode =
    PackagePageNode(
      p.name,
      contentBuilder.mkMemberInfo(p),
      JSet(p.dri),
      p,
      pagesForMembers(p),
      JNil
    )

  def pageForMember(c: Member): ClasslikePageNode = {
    val name =
      if c.kind == Kind.Object && c.companion.isDefined then
        c.getName + "$"
      else c.getName

    // Hack, need our own page!
    ClasslikePageNode(
      name,
      contentBuilder.mkMemberInfo(c),
      JSet(c.getDri),
      c.asInstanceOf[DClass],
      JNil,
      JNil,
    ).modified(name, pagesForMembers(c)) // We need override default page
  }

  override def contentForModule(m: DModule) = {
    def buildBlock = (builder: DocBuilder) => builder
      .group(kind = ContentKind.Cover) { gbuilder => gbuilder
        .cover(m.getName)()
        .descriptionIfNotEmpty(m)
      }
      .addChildren(contentForComments(m).asScala.toSeq)
      .groupingBlock(
        "Packages",
        List("" -> m.getPackages.asScala.toList.filter(_.allMembers.nonEmpty)),
        kind = ContentKind.Packages,
        sourceSets = m.getSourceSets.asScala.toSet
      )(
        (bdr, elem) => bdr
      ) { (bdr, elem) => bdr
        .driLink(elem.getName, elem.getDri)
      }

    contentBuilder.contentForDocumentable(m, buildBlock = buildBlock)
  }
  extension (b: DocBuilder)
    def descriptionIfNotEmpty(d: Documentable): DocBuilder = {
      val desc = this.contentForDescription(d).asScala.toSeq
      val res = if desc.isEmpty then b else b
        .sourceSetDependentHint(
          Set(d.getDri),
          d.getSourceSets.asScala.toSet,
          kind = ContentKind.SourceSetDependentHint,
          styles = Set(TextStyle.UnderCoverText)
        ) { sourceSetBuilder => sourceSetBuilder
            .addChildren(desc)
        }
      res
    }
