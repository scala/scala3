package dotty.tools
package dottydoc
package staticsite

import model.{ Entity, NonEntity }

import java.util.{ HashMap, List => JList, Map => JMap }
import scala.collection.JavaConverters._

case class DefaultParams(
  docs: JList[_],
  page: PageInfo,
  site: SiteInfo,
  sidebar: Sidebar,
  entity: Entity = NonEntity
) {
  import model.java._

  def toMap: Map[String, AnyRef] = Map(
    "docs" -> docs,

    "page" -> Map(
      "url" -> page.url,
      "date" -> page.date,
      "path" -> page.path
    ),

    "site" -> Map(
      "baseurl" -> site.baseurl,
      "posts" -> site.posts.map(_.toMap),
      "project" -> site.projectTitle
    ).asJava,

    "sidebar" -> sidebar.titles.asJava,

    "entity" -> entity.asJava()
  )

  def withPosts(posts: Array[BlogPost]): DefaultParams =
    copy(site = SiteInfo(site.baseurl, site.projectTitle, posts))

  def withUrl(url: String): DefaultParams =
    copy(page = PageInfo(url))

  def withEntity(e: model.Entity) = copy(entity = e)

  def withDate(d: String) = copy(page = PageInfo(page.url, d))
}

case class PageInfo(url: String, date: String = "") {
  val path: Array[String] = url.split('/').reverse.drop(1)
}

case class SiteInfo(baseurl: String, projectTitle: String, posts: Array[BlogPost])

case class Sidebar(titles: List[Title])

object Sidebar {
  def apply(map: HashMap[String, AnyRef]): Option[Sidebar] = Option(map.get("sidebar")).map {
    case list: JList[JMap[String, AnyRef]] @unchecked if !list.isEmpty =>
      new Sidebar(list.asScala.map(Title.apply).flatMap(x => x).toList)
    case _ => Sidebar.empty
  }

  def empty: Sidebar = Sidebar(Nil)
}

case class Title(title: String, url: Option[String], subsection: List[Title])

object Title {
  def apply(map: JMap[String, AnyRef]): Option[Title] = {
    val title = Option(map.get("title")).collect {
      case s: String => s
    }
    val url = Option(map.get("url")).collect {
      case s: String => s
    }
    val subsection = Option(map.get("subsection")).collect {
      case xs: JList[JMap[String, AnyRef]] @unchecked =>
        xs.asScala.map(Title.apply).toList.flatMap(x => x)
    }.getOrElse(Nil)

    title.map {
      case title: String  => Title(title, url, subsection)
    }
  }
}
