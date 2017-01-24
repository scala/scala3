package dotty.tools
package dottydoc
package staticsite

import java.io.{ File => JFile }
import java.util.{ List => JList, Map => JMap }

import dotc.config.Printers.dottydoc

import MapOperations._

/**
 *  A `BlogPost` represents the parsed posts from `./blog/_posts/`
 *  each post must be named according to the format
 *  `YYYY-MM-DD-title.{md,html}`
 */
class BlogPost(
  val title: String,
  val url: String,
  val date: String,
  val content: String,
  firstParagraph: String,
  val excerpt_separator: Option[String],
  val categories: JList[String]
) {
  import scala.collection.JavaConverters._
  lazy val excerpt: String = excerpt_separator match {
    case Some(str) => content.split(str).head
    case _ => firstParagraph
  }

  lazy val toMap: JMap[String, AnyRef] = Map(
    "title" -> title,
    "date" -> date,
    "url" -> url,
    "excerpt" -> excerpt,
    "excerpt_separator" -> excerpt_separator.getOrElse(""),
    "content" -> content,
    "categories" -> categories
  ).asJava
}

object BlogPost {
  val extract = """(\d\d\d\d)-(\d\d)-(\d\d)-(.*)\.(md|html)""".r
  def apply(file: JFile, page: Page): BlogPost = {
    def report(key: String, fallback: String = "") = {
      /*dottydoc.*/println(s"couldn't find page.$key in ${file.getName}")
      fallback
    }

    // Relying on the person using this class to pass a valid `file`
    val extract(year, month, day, name, _) = file.getName
    val title = page.yaml.getString("title").getOrElse(report("title", name))
    val url = page.yaml.getString("url").getOrElse(report("url"))
    val date = page.yaml.getString("date").getOrElse(s"$year-$month-$day 00:00:00")
    val excerptSep = page.yaml.getString("excerpt_separator")
    val categories = page.yaml.list("categories")

    new BlogPost(title, url, date, page.html, page.firstParagraph, excerptSep, categories)
  }
}
