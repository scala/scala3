package dotty.tools.scaladoc
package site

import com.vladsch.flexmark.util.{ast => mdu, sequence}
import com.vladsch.flexmark.{ast => mda}
import com.vladsch.flexmark.formatter.Formatter
import com.vladsch.flexmark.util.options.MutableDataSet
import scala.jdk.CollectionConverters._

import dotty.tools.scaladoc.tasty.comments.markdown.Section

object FlexmarkSectionWrapper {
  def apply(md: mdu.Document): mdu.Document = {
    val children = md.getChildren.asScala.toList
    val newChildren = getNewChildren(Nil, None, children)
    md.removeChildren()
    newChildren.foreach(md.appendChild)
    md
  }

  def getNewChildren(finished: List[mdu.Node], current: Option[(mda.Heading, List[mdu.Node])], rest: List[mdu.Node]): List[mdu.Node] = rest match {
    case Nil => current.fold(finished)(finished :+ Section(_, _))
    case (h: mda.Heading) :: rest => current.fold(getNewChildren(finished, Some(h, Nil), rest))((head, b) => getNewChildren(finished :+ Section(head, b), Some(h, Nil), rest))
    case (n: mdu.Node) :: rest => current.fold(getNewChildren(finished :+ n, None, rest))((head, b) => getNewChildren(finished, Some(head, b :+ n), rest))
  }
}
