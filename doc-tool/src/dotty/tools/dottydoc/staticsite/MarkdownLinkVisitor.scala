package dotty.tools
package dottydoc
package staticsite

import com.vladsch.flexmark.ast._
import com.vladsch.flexmark.util.ast._
import com.vladsch.flexmark.util.sequence.{BasedSequence, CharSubSequence}
import model.{Def, Package, TypeAlias, Val}
import dottydoc.util.MemberLookup

object MarkdownLinkVisitor {
  private val EntityLink = """([^\.]+)(\.[^\.]+)*""".r
  def apply(node: Node, docs: Map[String, Package], params: Map[String, AnyRef]): Unit =
    new NodeVisitor(
      new VisitHandler(classOf[Link], new Visitor[Link] with MemberLookup {
        override def visit(node: Link): Unit = {
          def isExternal(url: BasedSequence) =
            url.startsWith("http") || url.startsWith("https")

          val url = node.getUrl
          if (url.endsWith(".md") && !isExternal(url)) node.setUrl {
            url.subSequence(0, url.lastIndexOf('.')).append(".html")
          }
          else if (EntityLink.unapplySeq(url.toString).isDefined) {
            lookup(None, docs, url.toString).foreach { ent =>
              val (path, suffix) = ent match {
                case ent: Val => (ent.path.dropRight(1), ".html#" + ent.signature)
                case ent: Def => (ent.path.dropRight(1), ".html#" + ent.signature)
                case ent: TypeAlias => (ent.path.dropRight(1), ".html#" + ent.signature)
                case ent: Package => (ent.path, "/index.html")
                case ent => (ent.path, ".html")
              }

              params("site") match {
                case map: java.util.Map[String, String] @unchecked => node.setUrl {
                  CharSubSequence.of(path.mkString(map.get("baseurl") + "/api/", "/", suffix))
                }
                case _ => ()
              }
            }
          }
        }
      })
    )
    .visit(node)
}
