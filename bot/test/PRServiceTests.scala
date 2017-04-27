package dotty.tools.bot

import org.junit.Assert._
import org.junit.Test

import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.parser.decode

import model.Github._
import model.Drone
import org.http4s.client.blaze._
import org.http4s.client.Client
import scalaz.concurrent.Task

class PRServiceTests extends PullRequestService {
  val githubUser  = sys.env("GITHUB_USER")
  val githubToken = sys.env("GITHUB_TOKEN")
  val droneToken  = sys.env("DRONE_TOKEN")

  private def withClient[A](f: Client => Task[A]): A = {
    val httpClient = PooledHttp1Client()
    val ret = f(httpClient).run
    httpClient.shutdownNow()
    ret
  }

  def getResource(r: String): String =
    Option(getClass.getResourceAsStream(r)).map(scala.io.Source.fromInputStream)
      .map(_.mkString)
      .getOrElse(throw new Exception(s"resource not found: $r"))

  @Test def canUnmarshalIssueJson = {
    val json = getResource("/test-pr.json")
    val issue: Issue = decode[Issue](json) match {
      case Right(is: Issue) => is
      case Left(ex) => throw ex
    }

    assert(issue.pull_request.isDefined, "missing pull request")
  }

  @Test def canUnmarshalIssueComment = {
    val json = getResource("/test-mention.json")
    val issueComment: IssueComment = decode[IssueComment](json) match {
      case Right(is: IssueComment) => is
      case Left(ex) => throw ex
    }

    assert(
      issueComment.comment.body == "@dotty-bot: could you recheck this please?",
      s"incorrect body: ${issueComment.comment.body}"
    )
  }

  @Test def canGetAllCommitsFromPR = {
    val issueNbr = 1941 // has 2 commits: https://github.com/lampepfl/dotty/pull/1941/commits
    val List(c1, c2) = withClient(implicit client => getCommits(issueNbr))

    assertEquals(
      "Represent untyped operators as Ident instead of Name",
      c1.commit.message.takeWhile(_ != '\n')
    )

    assertEquals(
      "Better positions for infix term operations.",
      c2.commit.message.takeWhile(_ != '\n')
    )
  }

  @Test def canGetMoreThan100Commits = {
    val issueNbr = 1840 // has >100 commits: https://github.com/lampepfl/dotty/pull/1840/commits
    val numberOfCommits = withClient(implicit client => getCommits(issueNbr)).length

    assert(
      numberOfCommits > 100,
      s"PR 1840, should have a number of commits greater than 100, but was: $numberOfCommits"
    )
  }

  @Test def canGetComments = {
    val comments: List[Comment] = withClient(getComments(2136, _))

    assert(comments.find(_.user.login == Some("odersky")).isDefined,
           "Could not find Martin's comment on PR 2136")
  }

  @Test def canCheckCLA = {
    val validUserCommit = Commit("sha-here", Author(Some("felixmulder")), Author(Some("felixmulder")), CommitInfo(""))
    val statuses: List[CommitStatus] = withClient(checkCLA(validUserCommit :: Nil, _))

    assert(statuses.length == 1, s"wrong number of valid statuses: got ${statuses.length}, expected 1")
  }

  @Test def canSetStatus = {
    val sha = "fa64b4b613fe5e78a5b4185b4aeda89e2f1446ff"
    val status = Invalid("smarter", Commit(sha, Author(Some("smarter")), Author(Some("smarter")), CommitInfo("")))

    val statuses: List[StatusResponse] = withClient(sendStatuses(status :: Nil, _))

    assert(
      statuses.length == 1,
      s"assumed one status response would be returned, got: ${statuses.length}"
    )

    assert(
      statuses.head.state == "failure",
      s"status set had wrong state, expected 'failure', got: ${statuses.head.state}"
    )
  }

  @Test def canGetStatus = {
    val sha = "fa64b4b613fe5e78a5b4185b4aeda89e2f1446ff"
    val commit = Commit(sha, Author(None), Author(None), CommitInfo(""))
    val status = withClient(getStatus(commit, _)).head

    assert(status.sha == sha, "someting wong")
  }

  @Test def canPostReview = {
    val invalidUsers = "felixmulder" :: "smarter" :: Nil
    val commit = Commit("", Author(Some("smarter")), Author(Some("smarter")), CommitInfo("Added stuff"))
    val res = withClient(sendInitialComment(2281, invalidUsers, commit :: Nil, _))

    assert(
      res.body.contains("We want to keep history") &&
      res.body.contains("Could you folks please sign the CLA?") &&
      res.body.contains("Have an awesome day!"),
      s"Body of review was not as expected:\n${res.body}"
    )
  }

  @Test def canStartAndStopBuild = {
    val build = withClient(implicit client => Drone.startBuild(1921, droneToken))
    assert(build.status == "pending" || build.status == "building")
    val killed = withClient(implicit client => Drone.stopBuild(1921, droneToken))
    assert(killed, "Couldn't kill build")
  }

  @Test def canUnderstandWhenToRestartBuild = {
    val json = getResource("/test-mention.json")
    val issueComment: IssueComment = decode[IssueComment](json) match {
      case Right(is: IssueComment) => is
      case Left(ex) => throw ex
    }

    assert(respondToComment(issueComment).run.status.code == 200)
  }

  @Test def canTellUserWhenNotUnderstanding = {
    val json = getResource("/test-mention-no-understandy.json")
    val issueComment: IssueComment = decode[IssueComment](json) match {
      case Right(is: IssueComment) => is
      case Left(ex) => throw ex
    }

    assert(respondToComment(issueComment).run.status.code == 200)
  }
}
