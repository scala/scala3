package dotty.tools.dottydoc
package js
package html

import scalatags.JsDom.all._
import org.scalajs.dom
import org.scalajs.dom.html.{Anchor, Div}
import model._

case class EntityLayout(entity: Entity) extends MemberLayout {
  def html = div(
    cls := "page-content",
    div(raw(entity.comment.fold("")(_.body))),
    entity match {
      case e: Entity with Members =>
        Seq(
          h5("Members"),
          div(
            cls := "mld-grid",
            e.members
              .collect { case x: Entity with Modifiers if !x.isPrivate => x}
              .flatMap(member(_, entity))
          )
        )
      case _ => ()
    }
  )
}
