package dotty.tools.scaladoc

import org.scalajs.dom._
import org.scalajs.dom.ext._

import scala.util.matching.Regex._
import scala.util.matching._
import org.scalajs.dom._
import scala.scalajs.js
import scala.scalajs.js.JSON
import scala.scalajs.js.Thenable.Implicits.thenable2future

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Success,Failure}

import utils.HTML._

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
  def url: String

trait Commits extends js.Array[CommitTop]

trait CommitDescription extends js.Object:
  def files: js.Array[FileChange]

trait FileChange extends js.Object:
  def filename: String
  def status: String
  def previous_filename: String

class ContentContributors:
  val indenticonsUrl = "https://github.com/identicons"
  val htmlElement = window.document.documentElement
  def githubContributorsUrl() = htmlElement.getAttribute("data-githubContributorsUrl")
  def githubContributorsFilename() = htmlElement.getAttribute("data-githubContributorsFilename")
  def linkForFilename(filename: String) = githubContributorsUrl() + s"/commits?path=$filename"
  def getAuthorsForFilename(filename: String): Future[List[FullAuthor]] = {
    val link = linkForFilename(filename)
    fetch(link).flatMap(_.json()).flatMap { json =>
      val res = json.asInstanceOf[Commits]
      val authors = res.map { commit =>
        commit.author match
          case null =>
            FullAuthor(commit.commit.author.name, "", s"$indenticonsUrl/${commit.commit.author.name}.png")
          case author =>
            FullAuthor(author.login, author.html_url, author.avatar_url)
      }
      val lastCommit = res.lastOption
      val lastCommitDescriptionLink = lastCommit.map(_.url)
      val previousFilename = lastCommitDescriptionLink
        .fold(Future.successful(None)) { link =>
          findRename(link, filename)
        }
      val previousAuthors = previousFilename.flatMap {
        case Some(filename) => getAuthorsForFilename(filename)
        case None => Future.successful(List.empty)
      }.fallbackTo(Future.successful(List.empty))

      previousAuthors.map(_ ++ authors).map(_.distinct)
    }
  }
  def findRename(link: String, filename: String): Future[Option[String]] = {
    fetch(link).flatMap(_.json()).map { json =>
        val res = json.asInstanceOf[CommitDescription]
        val files = res.files
        files
          .find(_.filename == filename)
          .filter(_.status == "renamed")
          .map(_.previous_filename)
    }
  }
