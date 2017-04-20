package dotty.tools.bot

import org.http4s.{ Status => _, _ }
import org.http4s.client.blaze._
import org.http4s.client.Client
import org.http4s.headers.Authorization

import cats.syntax.applicative._
import scalaz.concurrent.Task
import scala.util.control.NonFatal
import scala.Function.tupled

import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s.circe._
import org.http4s.dsl._
import org.http4s.util._

import model.Github._

object TaskIsApplicative {
  implicit val taskIsApplicative = new cats.Applicative[Task] {
    def pure[A](x: A): Task[A] = Task.now(x)
    def ap[A, B](ff: Task[A => B])(fa: Task[A]): Task[B] =
      for(f <- ff; a <- fa) yield f(a)
  }
}
import TaskIsApplicative._

trait PullRequestService {

  /** Username for authorized admin */
  def user: String

  /** OAuth token needed for user to create statuses */
  def token: String

  /** Pull Request HTTP service */
  val prService = HttpService {
    case request @ POST -> Root =>
      request.as(jsonOf[Issue]).flatMap(checkPullRequest)
  }

  private[this] lazy val authHeader = {
    val creds = BasicCredentials(user, token)
    new Authorization(creds)
  }

  private final case class CLASignature(
    user: String,
    signed: Boolean,
    version: String,
    currentVersion: String
  )

  def claUrl(userName: String): String =
   s"https://www.lightbend.com/contribute/cla/scala/check/$userName"

  def commitsUrl(prNumber: Int): String =
    s"https://api.github.com/repos/lampepfl/dotty/pulls/$prNumber/commits?per_page=100"

  def statusUrl(sha: String): String =
    s"https://api.github.com/repos/lampepfl/dotty/statuses/$sha"

  def issueCommentsUrl(issueNbr: Int): String =
    s"https://api.github.com/repos/lampepfl/dotty/issues/$issueNbr/comments"

  def toUri(url: String): Task[Uri] =
    Uri.fromString(url).fold(Task.fail, Task.now)

  def getRequest(endpoint: Uri): Task[Request] =
    Request(uri = endpoint, method = Method.GET).putHeaders(authHeader).pure[Task]

  def postRequest(endpoint: Uri): Task[Request] =
    Request(uri = endpoint, method = Method.POST).putHeaders(authHeader).pure[Task]

  def shutdownClient(client: Client): Unit =
    client.shutdownNow()

  sealed trait CommitStatus {
    def commit: Commit
    def isValid: Boolean
  }
  final case class Valid(user: String, commit: Commit) extends CommitStatus { def isValid = true }
  final case class Invalid(user: String, commit: Commit) extends CommitStatus { def isValid = false }
  final case class CLAServiceDown(user: String, commit: Commit) extends CommitStatus { def isValid = false }
  final case class MissingUser(commit: Commit) extends CommitStatus { def isValid = false }

  /** Partitions invalid and valid commits */
  def checkCLA(xs: List[Commit], httpClient: Client): Task[List[CommitStatus]] = {
    def checkUser(user: String): Task[Commit => CommitStatus] = {
      val claStatus = for {
        endpoint <- toUri(claUrl(user))
        claReq   <- getRequest(endpoint)
        claRes   <- httpClient.expect(claReq)(jsonOf[CLASignature])
      } yield { (commit: Commit) =>
        if (claRes.signed) Valid(user, commit)
        else Invalid(user, commit)
      }

      claStatus.handleWith {
        case NonFatal(e) =>
          println(e)
          Task.now((commit: Commit) => CLAServiceDown(user, commit))
      }
    }

    def checkCommit(author: Author, commit: List[Commit]): Task[List[CommitStatus]] =
      author.login.map(checkUser)
        .getOrElse(Task.now(MissingUser))
        .map(f => commit.map(f))

    Task.gatherUnordered {
      val groupedByAuthor: Map[Author, List[Commit]] = xs.groupBy(_.author)
      groupedByAuthor.map(tupled(checkCommit)).toList
    }.map(_.flatten)
  }

  def sendStatuses(xs: List[CommitStatus], httpClient: Client): Task[List[StatusResponse]] = {
    def setStatus(cm: CommitStatus): Task[StatusResponse] = {
      val desc =
        if (cm.isValid) "User signed CLA"
        else "User needs to sign cla: https://www.lightbend.com/contribute/cla/scala"

      val stat = cm match {
        case Valid(user, commit) =>
          Status("success", claUrl(user), desc)
        case Invalid(user, commit) =>
          Status("failure", claUrl(user), desc)
        case MissingUser(commit) =>
          Status("failure", "", "Missing valid github user for this PR")
        case CLAServiceDown(user, commit) =>
          Status("pending", claUrl(user), "CLA Service is down")
      }

      for {
        endpoint <- toUri(statusUrl(cm.commit.sha))
        req      <- postRequest(endpoint).withBody(stat.asJson)
        res      <- httpClient.expect(req)(jsonOf[StatusResponse])
      } yield res
    }

    Task.gatherUnordered(xs.map(setStatus))
  }

  private[this] val ExtractLink = """<([^>]+)>; rel="([^"]+)"""".r
  def findNext(header: Option[Header]): Option[String] = header.flatMap { header =>
    val value = header.value

    value
      .split(',')
      .collect {
        case ExtractLink(url, kind) if kind == "next" =>
          url
      }
      .headOption
  }

  def getCommits(issueNbr: Int, httpClient: Client): Task[List[Commit]] = {
    def makeRequest(url: String): Task[List[Commit]] =
      for {
        endpoint <- toUri(url)
        req <- getRequest(endpoint)
        res <- httpClient.fetch(req){ res =>
          val link = CaseInsensitiveString("Link")
          val next = findNext(res.headers.get(link)).map(makeRequest).getOrElse(Task.now(Nil))

          res.as[List[Commit]](jsonOf[List[Commit]]).flatMap(commits => next.map(commits ++ _))
        }
      } yield res

    makeRequest(commitsUrl(issueNbr))
  }

  def getComments(issueNbr: Int, httpClient: Client): Task[List[Comment]] =
    for {
      endpoint <- toUri(issueCommentsUrl(issueNbr))
      req      <- getRequest(endpoint)
      res      <- httpClient.expect(req)(jsonOf[List[Comment]])
    } yield res

  def checkPullRequest(issue: Issue): Task[Response] = {
    val httpClient = PooledHttp1Client()

    for {
      // First get all the commits from the PR
      commits  <- getCommits(issue.number, httpClient)

      // Then check the CLA of each commit for both author and committer
      statuses <- checkCLA(commits, httpClient)

      // Send statuses to Github and exit
      _        <- sendStatuses(statuses, httpClient)
      _        <- shutdownClient(httpClient).pure[Task]
      resp     <- Ok("All statuses checked")
    } yield resp
  }
}
