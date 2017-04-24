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

  private[this] val githubUrl = "https://api.github.com"

  def claUrl(userName: String): String =
   s"https://www.lightbend.com/contribute/cla/scala/check/$userName"

  def commitsUrl(prNumber: Int): String =
    s"$githubUrl/repos/lampepfl/dotty/pulls/$prNumber/commits?per_page=100"

  def statusUrl(sha: String): String =
    s"$githubUrl/repos/lampepfl/dotty/statuses/$sha"

  def issueCommentsUrl(issueNbr: Int): String =
    s"$githubUrl/repos/lampepfl/dotty/issues/$issueNbr/comments"

  def toUri(url: String): Task[Uri] =
    Uri.fromString(url).fold(Task.fail, Task.now)

  def getRequest(endpoint: Uri): Task[Request] =
    Request(uri = endpoint, method = Method.GET).putHeaders(authHeader).pure[Task]

  def postRequest(endpoint: Uri): Task[Request] =
    Request(uri = endpoint, method = Method.POST).putHeaders(authHeader).pure[Task]

  def shutdownClient(client: Client): Task[Unit] =
    client.shutdownNow().pure[Task]

  sealed trait CommitStatus {
    def commit: Commit
    def isValid: Boolean
  }
  final case class Valid(user: Option[String], commit: Commit) extends CommitStatus { def isValid = true }
  final case class Invalid(user: String, commit: Commit) extends CommitStatus { def isValid = false }
  final case class CLAServiceDown(user: String, commit: Commit) extends CommitStatus { def isValid = false }
  final case class MissingUser(commit: Commit) extends CommitStatus { def isValid = false }
  final case class InvalidPrevious(users: List[String], commit: Commit) extends CommitStatus { def isValid = false }

  /** Partitions invalid and valid commits */
  def checkCLA(xs: List[Commit], httpClient: Client): Task[List[CommitStatus]] = {
    def checkUser(user: String): Task[Commit => CommitStatus] = {
      val claStatus = for {
        endpoint <- toUri(claUrl(user))
        claReq   <- getRequest(endpoint)
        claRes   <- httpClient.expect(claReq)(jsonOf[CLASignature])
      } yield { (commit: Commit) =>
        if (claRes.signed) Valid(Some(user), commit)
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

  def setStatus(cm: CommitStatus, httpClient: Client): Task[StatusResponse] = {
    val desc =
      if (cm.isValid) "User signed CLA"
      else "User needs to sign cla: https://www.lightbend.com/contribute/cla/scala"

    val stat = cm match {
      case Valid(Some(user), commit) =>
        Status("success", claUrl(user), desc)
      case Valid(None, commit) =>
        Status("success", "", "All contributors signed CLA")
      case Invalid(user, commit) =>
        Status("failure", claUrl(user), desc)
      case MissingUser(commit) =>
        Status("failure", "", "Missing valid github user for this PR")
      case CLAServiceDown(user, commit) =>
        Status("pending", claUrl(user), "CLA Service is down")
      case InvalidPrevious(users, latestCommit) =>
        Status("failure", "", users.map("@" + _).mkString(", ") + " have not signed the CLA")
    }

    for {
      endpoint <- toUri(statusUrl(cm.commit.sha))
      req      <- postRequest(endpoint).withBody(stat.asJson)
      res      <- httpClient.expect(req)(jsonOf[StatusResponse])
    } yield res
  }

  def sendStatuses(xs: List[CommitStatus], httpClient: Client): Task[List[StatusResponse]] =
    Task.gatherUnordered(xs.map(setStatus(_, httpClient)))

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

  /** Ordered from earliest to latest */
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


  private def usersFromInvalid(xs: List[CommitStatus]) =
    xs.collect { case Invalid(user, _) => user }

  def sendInitialComment(invalidUsers: List[String]): Task[Unit] = ???

  def checkFreshPR(issue: Issue): Task[Response] = {

    val httpClient = PooledHttp1Client()
    for {
      commits  <- getCommits(issue.number, httpClient)
      statuses <- checkCLA(commits, httpClient)

      (validStatuses, invalidStatuses) = statuses.partition(_.isValid)
      invalidUsers = usersFromInvalid(invalidStatuses)

      // Mark the invalid commits:
      _ <- sendStatuses(invalidStatuses, httpClient)

      // Set status of last to indicate previous failures or all good:
      _ <- {
        if (invalidStatuses.nonEmpty)
          setStatus(InvalidPrevious(invalidUsers, commits.last), httpClient)
        else
          setStatus(statuses.last, httpClient)
      }

      // Send positive comment:
      _    <- sendInitialComment(invalidUsers)
      _    <- shutdownClient(httpClient)
      resp <- Ok("Fresh PR checked")
    } yield resp

  }

  def getStatus(commit: Commit, client: Client): Task[StatusResponse] =
    for {
      endpoint <- toUri(statusUrl(commit.sha))
      req      <- getRequest(endpoint)
      res      <- client.expect(req)(jsonOf[List[StatusResponse]])
    } yield res.head

  def getStatuses(commits: List[Commit], client: Client): Task[List[StatusResponse]] =
    Task.gatherUnordered(commits.map(getStatus(_, client)))

  private def extractCommitSha(status: StatusResponse): Task[String] =
    Task.delay(status.sha)

  def recheckCLA(statuses: List[StatusResponse], commits: List[Commit], client: Client): Task[List[CommitStatus]] = {
    /** Return the matching commits from the SHAs */
    def prunedCommits(shas: List[String]): Task[List[Commit]] =
      Task.delay(commits.filter(cm => shas.contains(cm.sha)))

    for {
      commitShas <- Task.gatherUnordered(statuses.map(extractCommitSha))
      commits    <- prunedCommits(commitShas)
      statuses   <- checkCLA(commits, client)
    } yield statuses
  }

  def checkSynchronize(issue: Issue): Task[Response] = {
    val httpClient = PooledHttp1Client()

    for {
      commits  <- getCommits(issue.number, httpClient)
      statuses <- checkCLA(commits, httpClient)

      (_, invalid) = statuses.partition(_.isValid)

      _ <- sendStatuses(invalid, httpClient)

      // Set final commit status based on `invalid`:
      _ <- {
        if (invalid.nonEmpty)
          setStatus(InvalidPrevious(usersFromInvalid(invalid), commits.last), httpClient)
        else
          setStatus(statuses.last, httpClient)
      }
      _    <- shutdownClient(httpClient)
      resp <- Ok("Updated PR checked")
    } yield resp
  }

  def checkPullRequest(issue: Issue): Task[Response] =
    issue.action match {
      case "opened" => checkFreshPR(issue)
      case "synchronize" => checkSynchronize(issue)
      case action => BadRequest(s"Unhandled action: $action")
    }
}
