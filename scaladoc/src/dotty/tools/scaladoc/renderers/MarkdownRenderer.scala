package dotty.tools.scaladoc
package renderers

import util.HTML._

class MarkdownRenderer(rootPackage: Member, members: Map[DRI, Member])(using ctx: DocContext)
  extends Renderer(rootPackage, members, extension = "md"):

  override def render(): Unit =
    renderResources()
    super.render()

  override def pageContent(page: Page, parents: Vector[Link]): AppliedTag =
    renderContent(page).content

  private def renderResources(): Seq[String] =
    allResources(Nil).flatMap(renderResource)
