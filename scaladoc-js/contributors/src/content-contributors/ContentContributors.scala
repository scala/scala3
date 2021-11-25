package dotty.tools.scaladoc

import org.scalajs.dom._
import org.scalajs.dom.ext._

import scala.util.matching.Regex._
import scala.util.matching._
import org.scalajs.dom.ext.Ajax
import scala.scalajs.js.JSON
import scala.scalajs.js

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Success,Failure}

// Contributors widget
// see https://stackoverflow.com/a/19200303/4496364
// Copied from https://github.com/scala/docs.scala-lang/blob/main/resources/js/functions.js and rewritten to Scala.js

case class FullAuthor(name: String, url: String, image: String)

trait FallbackAuthor extends js.Object:
  def name: String

trait CommitBottom extends js.Object:
  def author: FallbackAuthor

trait Author extends js.Object:
  def login: String
  def avatar_url: String
  def html_url: String

trait CommitTop extends js.Object:
  def commit: CommitBottom
  def author: Author

trait Commits extends js.Array[CommitTop]

class ContentContributors:
  document.addEventListener("DOMContentLoaded", (e: Event) => {
    val indenticonsUrl = "https://github.com/identicons"
    js.typeOf(Globals.githubContributorsUrl) match
        case "undefined" =>
          // don't do anything
        case url =>
          val request: Future[String] = Ajax.get(Globals.githubContributorsUrl).map(_.responseText)
          request.onComplete {
            case Success(json: String) =>
              val res = JSON.parse(json).asInstanceOf[Commits]
              val authors = res.map { commit =>
                commit.author match
                  case null =>
                    FullAuthor(commit.commit.author.name, "", s"$indenticonsUrl/${commit.commit.author.name}.png")
                  case author =>
                    FullAuthor(author.login, author.html_url, author.avatar_url)
              }.distinct
              val maybeDiv = Option(document.getElementById("documentation-contributors"))
              maybeDiv.foreach { div =>
                authors.foreach { case FullAuthor(name, url, img) =>
                  val divN = document.createElement("div")
                  val imgN = document.createElement("img").asInstanceOf[html.Image]
                  imgN.src = img
                  val autN = document.createElement("a").asInstanceOf[html.Anchor]
                  autN.href = url
                  autN.text = name
                  divN.appendChild(imgN)
                  divN.appendChild(autN)
                  div.appendChild(divN)
                }

                if authors.nonEmpty then
                  div.asInstanceOf[html.Div].parentElement.classList.toggle("hidden")
              }
            case Failure(_) =>
              println(s"Couldn't fetch contributors for ${Globals.githubContributorsUrl}")
          }
  })

