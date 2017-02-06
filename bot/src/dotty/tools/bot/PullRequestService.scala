package dotty.tools.bot

import org.http4s._
import org.http4s.client.blaze._
import org.http4s.client.Client

import scalaz.concurrent.Task

import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s.circe._
import org.http4s.dsl._

import github4s.Github
import github4s.jvm.Implicits._
import github4s.free.domain.{ Commit, Issue }

trait PullRequestService {

  val prService = HttpService {
    case request @ POST -> Root =>
      request.as(jsonOf[Issue]).flatMap(checkPullRequest)
  }

  private case class CLASignature(
    user: String,
    signed: Boolean,
    version: String,
    currentVersion: String
  )

  private case class Status(
    state: String,
    target_url: String,
    description: String,
    context: String = "continuous-integration/CLA"
  )

  def claUrl(userName: String): String =
   s"https://www.lightbend.com/contribute/cla/scala/check/$userName"

  def commitsUrl(prNumber: Int): String =
    s"https://api.github.com/repos/lampepfl/dotty/pulls/$prNumber/commits"

  def toUri(url: String): Task[Uri] =
    Uri.fromString(url).fold(Task.fail, Task.now)

  def getRequest(endpoint: Uri): Task[Request] = Task.now {
    Request(uri = endpoint, method = Method.GET)
  }

  def postRequest(endpoint: Uri): Task[Request] = Task.now {
    Request(uri = endpoint, method = Method.POST)
  }

  def shutdownClient(client: Client): Task[Unit] = Task.now {
    client.shutdownNow()
  }

  def users(xs: List[Commit]): Task[Set[String]] = Task.now {
    xs.map(_.login).flatten.toSet
  }

  sealed trait CommitStatus {
    def commit: Commit
    def isValid: Boolean
  }
  final case class Valid(commit: Commit) extends CommitStatus { def isValid = true }
  final case class Invalid(commit: Commit) extends CommitStatus { def isValid = false }

  /** Partitions invalid and valid commits */
  def checkCLA(xs: List[Commit], httpClient: Client): Task[List[CommitStatus]] = {
    def checkUser(commit: Commit): Task[CommitStatus] = for {
      endpoint <- toUri(claUrl(commit.login.get))
      claReq   <- getRequest(endpoint)
      claRes   <- httpClient.expect(claReq)(jsonOf[CLASignature])
      res = if (claRes.signed) Valid(commit) else Invalid(commit)
    } yield res

    Task.gatherUnordered(xs.filter(_.login.isDefined).map(checkUser))
  }

  def sendStatuses(xs: List[CommitStatus], httpClient: Client): Task[Unit] = {
    def setStatus(cm: CommitStatus): Task[Unit] = for {
      endpoint <- toUri(cm.commit.url.replaceAll("git\\/commits", "statuses"))

      target = claUrl(cm.commit.login.getOrElse("<invalid-user>"))
      state = if (cm.isValid) "success" else "failure"
      desc =
        if (cm.isValid) "User signed CLA"
        else "User needs to sign cla: https://www.lightbend.com/contribute/cla/scala"

      statusReq <- postRequest(endpoint).map(_.withBody(Status(state, target, desc).asJson))
      statusRes <- httpClient.expect(statusReq)(jsonOf[String])
      print     <- Task.now(println(statusRes))
    } yield print

    Task.gatherUnordered(xs.map(setStatus)).map(_ => ())
  }

  def checkPullRequest(issue: Issue): Task[Response] = {
    val httpClient = PooledHttp1Client()

    for {
      // First get all the commits from the PR
      endpoint   <- toUri(commitsUrl(issue.number))
      commitsReq <- getRequest(endpoint)
      commitsRes <- httpClient.expect(commitsReq)(jsonOf[List[Commit]])

      // Then get check the CLA of each commit
      statuses   <- checkCLA(commitsRes, httpClient)

      // Send statuses to Github and exit
      _          <- sendStatuses(statuses, httpClient)
      _          <- shutdownClient(httpClient)
      resp       <- Ok("All statuses checked")
    } yield resp
  }
}
