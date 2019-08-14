package dotty.tools
package dottydoc
package staticsite

import model.{Entity, Package}

import java.util.{HashMap, List => JList, Map => JMap}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.collection.JavaConverters._

case class DefaultParams(
  docs: JList[_],
  docsFlattened: JList[_],
  originalDocs: Map[String, Package],
  page: PageInfo,
  site: SiteInfo,
  sidebar: Sidebar,
  entity: Option[Entity] = None
) {
  import model.JavaConverters._

  def toMap: Map[String, AnyRef] = {
    val base = Map(
      "docs" -> docs,

      "searchableDocs" -> docsFlattened,

      "originalDocs" -> originalDocs,

      "page" -> Map(
        "url" -> page.url,
        "date" -> page.date,
        "path" -> page.path
      ),

      "site" -> Map(
        "baseurl" -> site.baseurl,
        "posts" -> site.posts.map(_.toMap),
        "project" -> site.projectTitle,
        "version" -> site.projectVersion,
        "projectUrl" -> site.projectUrl.orNull,
        "logo" -> site.projectLogo.orNull,
        "root" -> site.root
      ).asJava,

      "sidebar" -> sidebar.toMap
    )
    val entityMap = entity match {
      case None => Map.empty
      case Some(entity) => Map(
        "entity" -> entity.asJava
      )
    }
    base ++ entityMap
  }

  def withPosts(posts: Array[BlogPost]): DefaultParams =
    copy(site = SiteInfo(
      site.baseurl, site.projectTitle, site.projectVersion, site.projectUrl, site.projectLogo,
      posts, site.root))

  def withUrl(url: String): DefaultParams =
    copy(page = PageInfo(url))

  def withEntity(e: Option[model.Entity]) = copy(entity = e)

  def withDate(d: String) = copy(page = PageInfo(page.url, d))
}

case class PageInfo(url: String, date: String = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd")).toString ) {
  val path: Array[String] = url.split('/').reverse.drop(1)
}

case class SiteInfo(
  baseurl: String,
  projectTitle: String,
  projectVersion: String,
  projectUrl: Option[String],
  projectLogo: Option[String],
  posts: Array[BlogPost],
  root: String
)

case class Sidebar(titles: List[Title]) {
  import model.JavaConverters._
  def toMap: JMap[String, _] =
    Map("titles" -> titles.map(_.toMap).asJava).asJava
}

object Sidebar {
  def apply(map: HashMap[String, AnyRef]): Option[Sidebar] = Option(map.get("sidebar")).map {
    case list: JList[JMap[String, AnyRef]] @unchecked if !list.isEmpty =>
      new Sidebar(list.asScala.map(Title.apply).flatten.toList)
    case _ => Sidebar.empty
  }

  def empty: Sidebar = Sidebar(Nil)
}

case class Title(title: String, url: Option[String], subsection: List[Title], description: Option[String]) {
  def toMap: JMap[String, _] = Map(
    "title" -> title,
    "url" -> url.orNull, // ugh, Java
    "subsection" -> subsection.map(_.toMap).asJava,
    "description" -> description.orNull
  ).asJava
}

object Title {
  def apply(map: JMap[String, AnyRef]): Option[Title] = {
    val title = Option(map.get("title")).collect {
      case s: String => s
    }
    val url = Option(map.get("url")).collect {
      case s: String => s
    }

    val description = Option(map.get("description")).collect {
      case s: String => s
    }

    val subsection = Option(map.get("subsection")).collect {
      case xs: JList[JMap[String, AnyRef]] @unchecked =>
        xs.asScala.map(Title.apply).toList.flatten
    }.getOrElse(Nil)

    title.map {
      case title: String  => Title(title, url, subsection, description)
    }
  }
}
