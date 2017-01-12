package dotty.tools
package dottydoc
package staticsite

import java.util.{ List => JList }
import model.{ Entity, NonEntity }

case class DefaultParams(
  docs: JList[_],
  page: PageInfo,
  site: SiteInfo,
  entity: Entity = NonEntity
) {
  import model.java._
  import scala.collection.JavaConverters._

  def toMap: Map[String, AnyRef] = Map(
    "docs" -> docs,

    "page" -> Map(
      "url" -> page.url,
      "path" -> page.path
    ),

    "site" -> Map(
      "baseurl" -> site.baseurl,
      "posts" -> site.posts.map(_.toMap)
    ).asJava,

    "entity" -> entity.asJava()
  )

  def withPosts(posts: Array[BlogPost]): DefaultParams =
    copy(site = SiteInfo(site.baseurl, posts))

  def withUrl(url: String): DefaultParams =
    copy(page = PageInfo(url))

  def withEntity(e: model.Entity) = copy(entity = e)
}

case class PageInfo(url: String) {
  val path: Array[String] = url.split('/').reverse.drop(1)
}

case class SiteInfo(baseurl: String, posts: Array[BlogPost])
