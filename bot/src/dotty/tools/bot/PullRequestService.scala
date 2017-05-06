package dotty.tools
package bot

import org.http4s.{ Status => _, _ }
import org.http4s.client.blaze._
import org.http4s.client.Client
import org.http4s.headers.{ Accept, Authorization }

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
import model.Drone
import bot.util.TaskIsApplicative._
import bot.util.HttpClientAux._

trait PullRequestService {

  /** Username for authorized admin */
  def githubUser: String

  /** OAuth token needed for user to create statuses */
  def githubToken: String

  /** OAuth token for drone, needed to cancel builds */
  def droneToken: String

  /** OAuthed application's "client_id" */
  def githubClientId: String

  /** OAuthed application's "client_secret" */
  def githubClientSecret: String

  /** Pull Request HTTP service */
  val prService = HttpService {
    case GET -> Root / "rate" => {
      val client = PooledHttp1Client()
      for {
        rates <- client.expect(get(rateLimit))(EntityDecoder.text)
        resp  <- Ok(rates)
        _     <- client.shutdown
      } yield resp
    }

    case request @ POST -> Root =>
      val githubEvent =
        request.headers
          .get(CaseInsensitiveString("X-GitHub-Event"))
          .map(_.value).getOrElse("")

      githubEvent match {
        case "pull_request" =>
          request.as(jsonOf[Issue]).flatMap(checkPullRequest)

        case "issue_comment" =>
          request.as(jsonOf[IssueComment]).flatMap(respondToComment)

        case "" =>
          BadRequest("Missing header: X-Github-Event")

        case event =>
          BadRequest("Unsupported event: $event")

      }
  }

  private[this] val droneContext = "continuous-integration/drone/pr"

  private final case class CLASignature(
    user: String,
    signed: Boolean,
    version: String,
    currentVersion: String
  )

  private[this] val githubUrl = "https://api.github.com"
  private[this] def withGithubSecret(url: String, extras: String*): String =
    s"$url?client_id=$githubClientId&client_secret=$githubClientSecret" + extras.mkString("&", "&", "")

  def rateLimit: String = withGithubSecret("https://api.github.com/rate_limit")

  def claUrl(userName: String): String =
   s"https://www.lightbend.com/contribute/cla/scala/check/$userName"

  def commitsUrl(prNumber: Int): String =
    withGithubSecret(s"$githubUrl/repos/lampepfl/dotty/pulls/$prNumber/commits", "per_page=100")

  def statusUrl(sha: String): String =
    withGithubSecret(s"$githubUrl/repos/lampepfl/dotty/statuses/$sha")

  def issueCommentsUrl(issueNbr: Int): String =
    withGithubSecret(s"$githubUrl/repos/lampepfl/dotty/issues/$issueNbr/comments")

  def reviewUrl(issueNbr: Int): String =
    withGithubSecret(s"$githubUrl/repos/lampepfl/dotty/pulls/$issueNbr/reviews")

  def contributorsUrl: String =
    withGithubSecret("https://api.github.com/repos/lampepfl/dotty/contributors")

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
        claRes <- httpClient.expect(get(claUrl(user)))(jsonOf[CLASignature])
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
      req <- post(statusUrl(cm.commit.sha)).withAuth(githubUser, githubToken)
      res <- httpClient.expect(req.withBody(stat.asJson))(jsonOf[StatusResponse])
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

  /** Get all contributors from GitHub */
  def getContributors(implicit client: Client): Task[Set[String]] =
    for {
      authors <- client.expect(get(contributorsUrl))(jsonOf[List[Author]])
      logins  =  authors.map(_.login).flatten
    } yield logins.toSet

  /** Ordered from earliest to latest */
  def getCommits(issueNbr: Int)(implicit httpClient: Client): Task[List[Commit]] = {
    def makeRequest(url: String): Task[List[Commit]] =
      for {
        res <- httpClient.fetch(get(url)) { res =>
          val link = CaseInsensitiveString("Link")
          val next = findNext(res.headers.get(link)).map(makeRequest).getOrElse(Task.now(Nil))

          res.as[List[Commit]](jsonOf[List[Commit]]).flatMap(commits => next.map(commits ++ _))
        }
      } yield res

    makeRequest(commitsUrl(issueNbr))
  }

  def getComments(issueNbr: Int, httpClient: Client): Task[List[Comment]] =
    httpClient.expect(get(issueCommentsUrl(issueNbr)))(jsonOf[List[Comment]])

  private def usersFromInvalid(xs: List[CommitStatus]) =
    xs.collect { case Invalid(user, _) => user }

  def hasBadCommitMessages(commits: List[Commit]): Boolean =
    commits.exists { cm =>
      val firstLine = cm.commit.message.takeWhile(_ != '\n').trim.toLowerCase
      val firstWord = firstLine.takeWhile(x => x != ':' && x != ' ')
      val containsColon = firstLine.contains(':')

      val wrongTense = firstWord match {
        case "added" | "fixed" | "removed" | "moved" | "changed" |
             "finalized" | "re-added"
        => true

        case "adds" | "fixes" | "removes" | "moves" | "changes" |
             "finalizes" | "re-adds"
        => true

        case _
        => false
      }

      wrongTense || firstLine.last == '.' || firstLine.length > 80
    }

  /** Returns the body of a `ReviewResponse` */
  def sendInitialComment(issueNbr: Int,
                         invalidUsers: List[String],
                         commits: List[Commit],
                         newContributors: Boolean)(implicit client: Client): Task[String] = {
    val cla = if (invalidUsers.nonEmpty) {
      s"""|## CLA ##
          |In order for us to be able to accept your contribution, all users
          |must sign the Scala CLA.
          |
          |Users:
          |${ invalidUsers.map("@" + _).mkString("- ", "\n- ", "") }
          |
          |Could you folks please sign the CLA? :pray:
          |
          |Please do this here: https://www.lightbend.com/contribute/cla/scala
          |""".stripMargin
    } else "All contributors have signed the CLA, thank you! :heart:"

    val commitRules = if (hasBadCommitMessages(commits)) {
      """|## Commit Messages ##
         |We want to keep history, but for that to actually be useful we have
         |some rules on how to format our commit messages ([relevant xkcd](https://xkcd.com/1296/)).
         |
         |Please stick to these guidelines for commit messages:
         |
         |> 1. Separate subject from body with a blank line
         |> 1. When fixing an issue, start your commit message with `Fix #<ISSUE-NBR>: `
         |> 1. Limit the subject line to 72 characters
         |> 1. Capitalize the subject line
         |> 1. Do not end the subject line with a period
         |> 1. Use the imperative mood in the subject line ("Added" instead of "Add")
         |> 1. Wrap the body at 80 characters
         |> 1. Use the body to explain what and why vs. how
         |>
         |> adapted from https://chris.beams.io/posts/git-commit""".stripMargin
    } else ""

    val body =
      s"""|Hello, and thank you for opening this PR! :tada:
          |
          |$cla
          |
          |$commitRules
          |
          |Have an awesome day! :sunny:""".stripMargin

    val review = Review.comment(body)

    val shouldPost = newContributors || commitRules.nonEmpty || invalidUsers.nonEmpty

    for {
      req <- post(reviewUrl(issueNbr)).withAuth(githubUser, githubToken)
      res <- {
        if (shouldPost)
          client.expect(req.withBody(review.asJson))(jsonOf[ReviewResponse]).map(_.body)
        else
          Task.now("")
      }
    } yield res
  }

  def checkFreshPR(issue: Issue): Task[Response] = {
    implicit val httpClient = PooledHttp1Client()

    for {
      commits  <- getCommits(issue.number)
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

      authors  =  commits.map(_.author.login).flatten.toSet
      contribs <- getContributors
      newContr =  !authors.forall(contribs.contains)
      _        <- sendInitialComment(issue.number, invalidUsers, commits, newContr)
      _        <- httpClient.shutdown
      resp     <- Ok("Fresh PR checked")
    } yield resp

  }

  def getStatus(commit: Commit, client: Client): Task[List[StatusResponse]] =
    client.expect(get(statusUrl(commit.sha)))(jsonOf[List[StatusResponse]])

  private def extractCommitSha(status: StatusResponse): Task[String] =
    Task.delay(status.sha)

  def startBuild(commit: Commit)(implicit client: Client): Task[Drone.Build] = {
    def pendingStatus(targetUrl: String): Status =
      Status("pending", targetUrl, "build restarted by bot", droneContext)

    def filterStatuses(xs: List[StatusResponse]): Task[Int] =
      xs.filter { status =>
        (status.state == "failure" || status.state == "success") &&
        status.context == droneContext
      }
      .map(status => Task.now(status.target_url.split('/').last.toInt))
      .headOption
      .getOrElse(Task.fail(new NoSuchElementException("Couldn't find drone build for PR")))

    for {
      statuses     <- getStatus(commit, client)
      failed       <- filterStatuses(statuses)
      build        <- Drone.startBuild(failed, droneToken)
      setStatusReq <- post(statusUrl(commit.sha)).withAuth(githubUser, githubToken)
      newStatus    =  pendingStatus(s"http://dotty-ci.epfl.ch/lampepfl/dotty/$failed").asJson
      _            <- client.expect(setStatusReq.withBody(newStatus))(jsonOf[StatusResponse])
    } yield build
  }

  def cancelBuilds(commits: List[Commit])(implicit client: Client): Task[Boolean] =
    Task.gatherUnordered {
      commits.map { commit =>
        for {
          statuses    <- getStatus(commit, client)
          cancellable =  statuses.filter(status => status.state == "pending" && status.context == droneContext)
          runningJobs =  cancellable.map(_.target_url.split('/').last.toInt)
          cancelled   <- Task.gatherUnordered(runningJobs.map(Drone.stopBuild(_, droneToken)))
        } yield cancelled.forall(identity)
      }
    }
    .map(_.forall(identity))

  def checkSynchronize(issue: Issue): Task[Response] = {
    implicit val httpClient = PooledHttp1Client()

    for {
      commits  <- getCommits(issue.number)
      statuses <- checkCLA(commits, httpClient)
      invalid  =  statuses.filterNot(_.isValid)
      _        <- sendStatuses(invalid, httpClient)
      _        <- cancelBuilds(commits.dropRight(1))(httpClient)

      // Set final commit status based on `invalid`:
      _ <- {
        if (invalid.nonEmpty)
          setStatus(InvalidPrevious(usersFromInvalid(invalid), commits.last), httpClient)
        else
          setStatus(statuses.last, httpClient)
      }
      _    <- httpClient.shutdown
      resp <- Ok("Updated PR checked")
    } yield resp
  }

  def checkPullRequest(issue: Issue): Task[Response] =
    issue.action match {
      case Some("opened") => checkFreshPR(issue)
      case Some("synchronize") => checkSynchronize(issue)
      case Some(action) => BadRequest(s"Unhandled action: $action")
      case None => BadRequest("Cannot check pull request, missing action field")
    }

  def restartCI(issue: Issue): Task[Response] = {
    implicit val client = PooledHttp1Client()

    def restartedComment: Comment = {
      import scala.util.Random
      val answers = Array(
        "Okidokey, boss! :clap:",
        "You got it, homie! :pray:",
        "No problem, big shot! :punch:",
        "Sure thing, I got your back! :heart:",
        "No WAY! :-1: ...wait, don't fire me please! There, I did it! :tada:"
      )

      Comment(Author(None), answers(Random.nextInt(answers.length)))
    }

    for {
      commits <- getCommits(issue.number)
      latest  =  commits.last
      _       <- cancelBuilds(latest :: Nil)
      _       <- startBuild(latest)
      req     <- post(issueCommentsUrl(issue.number)).withAuth(githubUser, githubToken)
      _       <- client.fetch(req.withBody(restartedComment.asJson))(Task.now)
      res     <- Ok("Replied to request for CI restart")
    } yield res
  }

  def cannotUnderstand(line: String, issueComment: IssueComment): Task[Response] = {
    implicit val client = PooledHttp1Client()
    val comment = Comment(Author(None), {
      s"""Hey, sorry - I could not understand what you meant by:
         |
         |> $line
         |
         |I'm just a dumb bot after all :cry:
         |
         |I mostly understand when your mention contains these words:
         |
         |- (re)check (the) cla
         |- recheck
         |- restart drone
         |
         |Maybe if you want to make me smarter, you could open a PR? :heart_eyes:
         |""".stripMargin
    })
    for {
      req <- post(issueCommentsUrl(issueComment.issue.number)).withAuth(githubUser, githubToken)
      _   <- client.fetch(req.withBody(comment.asJson))(Task.now)
      res <- Ok("Delivered could not understand comment")
    } yield res
  }

  def extractMention(body: String): Option[String] =
    body.lines.find(_.startsWith("@dotty-bot:"))

  /** Try to make sense of what the user is requesting from the bot
   *
   *  The bot's abilities currently only include:
   *
   *  - Checking or re-checking the CLA
   *  - Restarting the CI tests
   *
   *  @note The implementation here could be quite elegant if we used a trie
   *        instead
   */
  def interpretMention(line: String, issueComment: IssueComment): Task[Response] = {
    val loweredLine = line.toLowerCase
    if (loweredLine.contains("check cla") || loweredLine.contains("check the cla"))
      checkSynchronize(issueComment.issue)
    else if (loweredLine.contains("recheck") || loweredLine.contains("restart drone"))
      restartCI(issueComment.issue)
    else
      cannotUnderstand(line, issueComment)
  }

  def respondToComment(issueComment: IssueComment): Task[Response] =
    extractMention(issueComment.comment.body)
      .map(interpretMention(_, issueComment))
      .getOrElse(Ok("Nothing to do here, move along!"))
}
