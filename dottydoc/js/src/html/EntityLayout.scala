package dotty.tools.dottydoc
package js
package html

import scalatags.JsDom.all._
import org.scalajs.dom
import org.scalajs.dom.html.{Anchor, Div}
import dotty.tools.dottydoc.model._
import dotty.tools.dottydoc.model.comment.{ UnsetLink, Text }

case class EntityLayout(entity: Entity) extends MemberLayout {
  def html = div(
    cls := "page-content",
    div(cls := "entity-title", entityTitle),
    div(raw(entity.comment.fold("")(_.body))),
    entity match {
      case e: Entity with Members if e.members.nonEmpty =>
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

  def entityTitle = {
    val modifiers = entity match {
      case m: Modifiers if m.modifiers.nonEmpty =>
        Some(span(cls := "entity-modifiers", m.modifiers.mkString(" ")))
      case _ => None
    }

    val typeParams = entity match {
      case t: TypeParams if t.typeParams.nonEmpty =>
        Some(span(cls := "entity-type-params no-left-margin", t.typeParams.mkString("[", ", ", "]")))
      case _ => None
    }

    val superTypes = entity match {
      case st: SuperTypes => Some(span(
        cls := "entity-super-types",
        st.superTypes.collect {
          case unset: UnsetLink => unset.title.asInstanceOf[Text].text
        }.mkString("extends ", " with ", "")
      ))
      case _ => None
    }

    modifiers ::
    Some(span(cls := "entity-kind", entity.kind)) ::
    Some(span(cls := "entity-name", entity.name)) ::
    typeParams ::
    superTypes ::
    Nil

  }.flatten
}
