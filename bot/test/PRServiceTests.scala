package dotty.tools.bot

import org.junit.Assert._
import org.junit.Test

import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.parser.decode

import model.Github._
import org.http4s.client.blaze._
import org.http4s.client.Client
import scalaz.concurrent.Task

class PRServiceTests extends PullRequestService {
  val user = sys.env("USER")
  val token = sys.env("TOKEN")

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

  @Test def canGetAllCommitsFromPR = {
    val issueNbr = 1941 // has 2 commits: https://github.com/lampepfl/dotty/pull/1941/commits
    val List(c1, c2) = withClient(getCommits(issueNbr, _))

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
    val numberOfCommits = withClient(getCommits(issueNbr, _)).length

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
    val status = withClient(getStatus(commit, _))

    assert(status.sha == sha, "someting wong")
  }

  @Test def canRecheckCLA = {
    val shas =
      "1d62587cb3f41dafd796b0c92ec1c22d95b879f9" ::
      "ad60a386f488a16612c093576bf7bf4d9f0073bf" ::
      Nil

    val commits = shas.map { sha =>
      Commit(sha, Author(Some("felixmulder")), Author(Some("felixmulder")), CommitInfo(""))
    }

    val statuses = shas.map { sha =>
      StatusResponse("https://api.github.com/repos/lampepfl/dotty/statuses/" + sha, 0, "failure")
    }

    val rechecked = withClient(recheckCLA(statuses, commits, _))

    assert(rechecked.forall(cs => cs.isValid), s"Should have set all statuses to valid, but got: $rechecked")
  }
}
