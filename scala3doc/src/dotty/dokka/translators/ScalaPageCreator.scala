package dotty.dokka

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.chaining._
import org.jetbrains.dokka.base.translators.documentables.DefaultPageCreator
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

class ScalaPageCreator(
  commentsToContentConverter: CommentsToContentConverter,
  signatureProvider: SignatureProvider,
)(using ctx: DocContext)
  extends DefaultPageCreator(commentsToContentConverter, signatureProvider, ctx.logger):

  def mkMemberInfo(m: Member) = MemberInfo(m, ContentNodeParams(
      new DCI(JSet(m.dri), ContentKind.Main),
      m.getSourceSets.asScala.toSet.toDisplay,
      Set(),
      PropertyContainer.Companion.empty()
  ))

  override def pageForModule(m: DModule): ModulePageNode =
    val rootPackage = m.getPackages.get(0)
    new ModulePageNode(
      m.getName,
      mkMemberInfo(rootPackage),
      m,
      pagesForMembers(rootPackage),
      JNil
    )

  private def pagesForMembers(member: Member): JList[PageNode] =
    val all = member
      .membersBy(m => m.kind == Kind.Package || m.kind.isInstanceOf[Classlike])
      .filter(m => m.origin == Origin.RegularlyDefined && m.inheritedFrom.isEmpty)
    all.map(pageForMember(_)).asJava

  def pageForMember(c: Member): ClasslikePageNode = {
    val name =
      if c.kind == Kind.Object && c.companion.isDefined then
        c.getName + "$"
      else c.getName

    // Hack, need our own page!
    ClasslikePageNode(
      name,
      mkMemberInfo(c),
      JSet(c.getDri),
      c.asInstanceOf[DClass],
      JNil,
      JNil,
    ).modified(name, pagesForMembers(c)) // We need override default page
  }
