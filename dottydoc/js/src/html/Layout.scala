package dotty.tools.dottydoc
package js
package html

import scalatags.JsDom.all._

object Index {
  import model.Entities._
  import CustomTags._

  def layout(ent: Entity) = ent match {
    case x: Entity with Members with Modifiers => packageMember(x)
  }

  def packageMember(m: Entity with Members with Modifiers) =
    div(
      cls := "mdl-layout mdl-js-layout mdl-layout--fixed-drawer",
      div(
        cls := "mdl-layout__drawer",
        span(
          cls := "mdl-layout-title subtitle",
          m.path.dropRight(1).mkString(".")
        ),
        span(
          cls := "mdl-layout-title",
          m.name
        ),
        nav(
          cls := "mdl-navigation",
          a(cls := "mdl-navigation__link", href := companionHref(m).map(_.path.last + ".html").getOrElse(""), "Companion " + m.name),
          a(cls := "mdl-navigation__link", href := m.sourceUrl, "Source")
        ),
        span(
          cls := "mdl-layout-title",
          "Packages"
        ),
        packageView
      ),
      main(
        cls := "mdl-layout__content",
        div(
          cls := "page-content",
          div(raw(m.comment.fold("")(_.body))),
          div(
            cls := "mld-grid",
            m.members
             .collect { case x: Entity with Modifiers if !x.isPrivate => x}
             .flatMap(member)
          )
        )
      )
    )

  def packageView = nav(
    cls := "mdl-navigation",
    ParsedIndex.packages.keys.flatMap { k =>
      ParsedIndex.packages(k).children.sortBy(_.name).map { c =>
        a(cls := "mdl-navigation__link", href := "#", c.name)
      }
    }.toList
  )

  def companionHref(m: Entity): Option[PackageMember] = {
    val pack = m.path.dropRight(1).mkString(".")
    ParsedIndex.packages.get(pack)
      .flatMap(_.children.find(e => e.name == m.name && e.path.last != m.path.last))
  }

  def member(m: Entity) = m match {
    case m: Entity with Modifiers => Seq(
        div(
          cls := "mdl-cell mdl-cell--12-col",
          h6(m.modifiers.mkString(" ") + " " + m.kind + " " + m.name)
        ),
        div(
          cls := "mdl-cell mdl-cell--12-col",
          raw(m.comment.fold("")(_.body)))
    )
  }
}
