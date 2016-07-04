package dotty.tools.dottydoc
package js
package html

import scalatags.JsDom.all._
import org.scalajs.dom
import org.scalajs.dom.html.{Anchor, Div}
import js.model._
import js.model.ops._

case class EntityLayout(entity: Entity) extends MemberLayout {
  import CustomTags._
  import EntityIndex.packages

  private def relativePath(to: Entity) = {
    val offset = entity.kind match {
      case "val" | "def" => 2
      case _ => 1
    }

    val prefix = entity.kind match {
      case "package" => "../"
      case _ => ""
    }

    prefix + {
      "../" * (entity.path.length - offset) + to.path.mkString("","/",".html")
    }
  }

  def html =
    div(
      cls := "mdl-layout mdl-js-layout mdl-layout--fixed-drawer",
      div(
        cls := "mdl-layout__drawer",
        span(
          cls := "mdl-layout-title subtitle",
          entity.path.dropRight(1).mkString(".")
        ),
        span(
          cls := "mdl-layout-title",
          if (entity.kind == "package") entity.name.split("\\.").last
          else entity.name
        ),
        nav(
          cls := "related mdl-navigation",
          companionAnchor,
          a(cls := "mdl-navigation__link", href := "#", "Source")
        ),
        span(
          cls := "mdl-layout-title",
          id := "docs-title",
          "Docs"
        ),
        searchView,
        packageView
      ),
      main(
        id := "entity-container",
        cls := "mdl-layout__content",
        div(
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
      ),
      main(
        id := "search-results",
        cls := "mdl-layout__content"
      )
    )

  def packageView = ul(
    cls := "mdl-list packages",
    {
      val keys: Seq[String] = packages.keys.toSeq.sorted
      val productReg = """^Product[0-9]+$""".r
      keys.flatMap { k =>
        val pack = packages(k)
        val children =
          pack.children
            .sortBy(_.name)
            .filterNot { ent =>
              // Filter out ProductX where X > 3
              productReg.findFirstIn(ent.name).map(_.slice(7, 99).toInt > 3).getOrElse(false) ||
              // Filter out packages
              ent.kind == "package" ||
              // Filter out objects that have companions
              (ent.kind == "object" && companion(ent).isDefined) ||
              ent.name == "AnyValCompanion"
            }
            .map { entity =>
              val comp = companion(entity)
              val entityUrl = relativePath(entity)
              val compUrl = comp.map(relativePath).getOrElse("#")
              li(
                cls := s"""mdl-list__item entity ${ if (comp.isDefined) "two" else "one" }""",
                comp.map { _ => a(cls := "entity-button object", href := compUrl, "O") }.getOrElse(()),
                a(
                  cls := s"""entity-button shadowed ${entity.kind.replaceAll(" ", "")}""",
                  href := entityUrl,
                  entity.kind(0).toUpper.toString
                ),
                a(
                  cls := "entity-name",
                  href := entityUrl,
                  entity.name
                )
              )
            }

        if (children.length > 0)
          li(cls := "mdl-list__item package", href := relativePath(pack), k) :: children.toList
        else Nil
      }
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

  def companion(entity: Entity) = {
    val pack = entity.path.dropRight(1).mkString(".")
    for {
      p     <- packages.get(pack)
      child <- p.children.find(e => e.name == entity.name && e.path.last != entity.path.last)
    } yield child
  }

  def companionAnchor =
    companion(entity).map { c =>
      a(
        cls := "mdl-navigation__link",
        href := c.path.last + ".html",
        "Companion " + c.kind
      )
    }.getOrElse(span())

  def searchView =
    div(
      cls := "search-container",
      div(
        cls := "mdl-textfield mdl-js-textfield mdl-textfield--floating-label",
        input(cls := "mdl-textfield__input", `type` := "text", id := "search"),
        label(cls := "mdl-textfield__label", `for` := "search", "Search")
      )
    )
}
