package dotty.tools
package dottydoc
package staticsite

import java.util.{ List => JList }

case class DefaultParams(
  docs: JList[_],
  page: PageInfo,
  site: SiteInfo
) {

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
    ).asJava
  )

  def withPosts(posts: Array[BlogPost]): DefaultParams =
    copy(site = SiteInfo(site.baseurl, posts))

  def withUrl(url: String): DefaultParams =
    copy(page = PageInfo(url))
}

case class PageInfo(url: String) {
  val path: Array[String] = url.split('/').reverse.drop(1)
}

case class SiteInfo(baseurl: String, posts: Array[BlogPost])
