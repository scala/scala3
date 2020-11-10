package dotty.dokka

import com.fasterxml.jackson.module.kotlin.ExtensionsKt._
import org.jetbrains.dokka.base.renderers.html.{SearchbarDataInstaller, SearchRecord}
import java.util.{List => JList}
import java.util.concurrent.ConcurrentHashMap
import collection.JavaConverters._
import org.jetbrains.dokka.pages._
import dotty.dokka.model.api._
import org.jetbrains.dokka.plugability._
import org.jetbrains.dokka.plugability.DokkaContext
import org.jetbrains.dokka.plugability.DokkaPluginKt._
import org.jetbrains.dokka.base.DokkaBase
import dotty.dokka.PluginUtils._
import org.jetbrains.dokka.base.signatures.SignatureProvider
import org.jetbrains.dokka._
import org.jetbrains.dokka.model._
import scala.collection.concurrent.TrieMap
import dotty.dokka.site.StaticPageNode

class ScalaSearchbarDataInstaller(val ctx: DokkaContext) extends SearchbarDataInstaller:

  case class PageEntry(val name: String, val signature: String, val link: String, val pkg: String)

  // We need to use there mutable, concurrent collections because Dokka renders content concurrently 
  // and adds entry to searchbar on start of processing page
  val pages = TrieMap[String, PageEntry]()

  val signatureProvider = querySingle[DokkaBase, SignatureProvider](ctx, _.getSignatureProvider)

  override def processPage(page: ContentPage, link: String) =
    Option(page.getDocumentable) match {
      case Some(member) => processMember(member, link)
      case None => page match {
        case p: StaticPageNode => processStaticSite(p, link)
        case _ => ()
      }
    }

  def flattenToText(node: ContentNode): String = {
    def getContentTextNodes(node: ContentNode, sourceSetRestriciton: DisplaySourceSet): List[ContentText] = node match {
      case t: ContentText => List(t)
      case c: ContentComposite if c.getDci.getKind != ContentKind.Annotations => c.getChildren.asScala
        .filter(_.getSourceSets.asScala.contains(sourceSetRestriciton))
        .flatMap(getContentTextNodes(_, sourceSetRestriciton))
        .toList
      case _ => List.empty
    }

    val sourceSetRestriciton = node.getSourceSets.asScala.find(_.getPlatform == Platform.common).getOrElse(node.getSourceSets.asScala.head)
    getContentTextNodes(node, sourceSetRestriciton).map(_.getText).mkString("")
  }

  def processMember(member: Member, link: String) = {
    val memberSignature = flattenToText(signatureProvider.signature(member).get(0))
    val memberPackage = (Option(member.dri.getPackageName) ++ Option(member.dri.getClassNames) ++ Option(member.dri.getCallable)).mkString(".")
    pages.addOne(memberSignature + link, PageEntry(member.name, memberSignature, link, memberPackage))
  }

  def processStaticSite(p: StaticPageNode, link: String) = {
    pages.addOne(p.getName + link, PageEntry(p.getName, p.getName, link, ""))
  }

  override def generatePagesList(): String = {
    val mapper = jacksonObjectMapper()
    val pagesList = pages.values.map(p => createSearchRecord(p.signature, p.pkg, p.link, List(p.name).asJava)).toList.asJava
    mapper.writeValueAsString(pagesList)
  }
