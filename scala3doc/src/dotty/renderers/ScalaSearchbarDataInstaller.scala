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


  override def processPage(page: ContentPage, link: String) =
    Option(page.getDocumentable) match {
      case Some(member) => {
        // All members that don't have their own page
        val all = member
          .membersBy(m => m.kind != dotty.dokka.model.api.Kind.Package && !m.kind.isInstanceOf[Classlike])
          .filter(m => m.origin == Origin.RegularlyDefined && m.inheritedFrom.isEmpty)
        all.foreach(processMember(_, link))
        processMember(member, link)
      }
      case None => page match {
        case p: StaticPageNode => processStaticSite(p, link)
        case _ => ()
      }
    }

  def flattenToText(signature: Signature): String =
    signature.map {
      case Link(name, dri) => name
      case s: String => s
    }.mkString

  def processMember(member: Member, link: String) = {
    val signatureBuilder = ScalaSignatureProvider.rawSignature(member, InlineSignatureBuilder()).asInstanceOf[InlineSignatureBuilder]
    val memberSignature = flattenToText(Seq(signatureBuilder.preName.head) ++ Seq(Link(member.name, member.dri)) ++ signatureBuilder.names.reverse)
    val memberPackage = Option(member.dri.getPackageName).mkString
    pages.addOne(memberSignature + link, PageEntry(member.name, memberSignature, link, memberPackage))
  }

  def processStaticSite(p: StaticPageNode, link: String) = {
    pages.addOne(p.getName + link, PageEntry(p.getName, p.getName, link, ""))
  }

  private def createAcronym(s: String): Option[String] =
    if s.head.isUpper then Some(s.filter(_.isUpper)) else None

  override def generatePagesList(): String = {
    val mapper = jacksonObjectMapper()
    val pagesList = pages.values.map(p => createSearchRecord(p.signature, p.pkg, p.link, (List(p.name) ++ createAcronym(p.name)).asJava)).toList.asJava
    mapper.writeValueAsString(pagesList)
  }
