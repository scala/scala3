package dotty.tools.dottydoc
package js
package html

import scalatags.JsDom.all._
import org.scalajs.dom
import org.scalajs.dom.html.{Anchor, Div}
import js.model._
import js.model.ops._

case class EntityLayout(entity: Entity) extends MemberLayout {
  def html = div(
    cls := "page-content",
    div(cls := "entity-title", entityTitle),
    div(raw(entity.comment.fold("")(_.body))),
    entity match {
      case x if x.hasMembers =>
        val e = x.asInstanceOf[Entity with Members]
        if (e.members.nonEmpty)
          Seq(
            h5("Members"),
            div(
              cls := "mld-grid",
              e.members
                .collect {
                  case x if x.hasModifiers && !x.isPrivate => x
                }
                .flatMap(member(_, entity)).toList
            )
          )
      case _ => ()
    }
  )

  def entityTitle = {
    val modifiers = entity.kind match {
      case "Modifiers" =>
        val m = entity.asInstanceOf[Modifiers]
        if (m.modifiers.nonEmpty)
          Some(span(cls := "entity-modifiers", m.modifiers.mkString(" ")))
        else
          None
      case _ => None
    }

    val typeParams = entity match {
      case x if x.hasTypeParams =>
        val t = entity.asInstanceOf[TypeParams]
        if (t.typeParams.nonEmpty)
          Some(span(cls := "entity-type-params no-left-margin", t.typeParams.mkString("[", ", ", "]")))
        else None
      case _ => None
    }

    val superTypes = entity match {
      case x if x.hasSuperTypes =>
        val st = entity.asInstanceOf[SuperTypes]
        if (st.superTypes.nonEmpty)
          Some(span(
            cls := "entity-super-types",
            st.superTypes.collect {
              case x => x.title
            }.mkString("extends ", " with ", "")
          ))
        else None
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
