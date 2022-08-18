package dotty.tools.scaladoc
package renderers

import util.HTML._
import scala.jdk.CollectionConverters._
import java.net.URI
import java.net.URL
import dotty.tools.scaladoc.site._
import scala.util.Try
import org.jsoup.Jsoup
import java.nio.file.Paths
import java.nio.file.Path
import java.nio.file.Files
import java.nio.file.FileVisitOption
import java.io.File

class MarkdownRenderer(rootPackage: Member, members: Map[DRI, Member])(using ctx: DocContext)
  extends Renderer(rootPackage, members, extension = "md"):

  override def render(): Unit =
    renderResources()
    super.render()

  override def pageContent(page: Page, parents: Vector[Link]): AppliedTag =
    renderContent(page).content

  private def renderResources(): Seq[String] =
    allResources(Nil).flatMap(renderResource)
