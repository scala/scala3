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
    val httpClient = PooledHttp1Client()
    val issueNbr = 1941 // has 2 commits: https://github.com/lampepfl/dotty/pull/1941/commits

    val List(c1, c2) = getCommits(issueNbr, httpClient).run

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
    val httpClient = PooledHttp1Client()
    val issueNbr = 1840 // has >100 commits: https://github.com/lampepfl/dotty/pull/1840/commits

    val numberOfCommits = getCommits(issueNbr, httpClient).run.length

    assert(
      numberOfCommits > 100,
      s"PR 1840, should have a number of commits greater than 100, but was: $numberOfCommits"
    )
  }

  @Test def canGetComments = {
    val comments: List[Comment] = withClient(c => getComments(2136, c))

    assert(comments.find(_.user.login == Some("odersky")).isDefined,
           "Could not find Martin's comment on PR 2136")
  }

  @Test def canCheckCLA = {
    val httpClient = PooledHttp1Client()
    val validUserCommit = Commit("sha-here", Author(Some("felixmulder")), Author(Some("felixmulder")), CommitInfo(""))
    val statuses: List[CommitStatus] = checkCLA(validUserCommit :: Nil, httpClient).run

    assert(statuses.length == 1, s"wrong number of valid statuses: got ${statuses.length}, expected 1")
    httpClient.shutdownNow()
  }

  @Test def canSetStatus = {
    val httpClient = PooledHttp1Client()
    val sha = "fa64b4b613fe5e78a5b4185b4aeda89e2f1446ff"
    val status = Invalid("smarter", Commit(sha, Author(Some("smarter")), Author(Some("smarter")), CommitInfo("")))

    val statuses: List[StatusResponse] = sendStatuses(status :: Nil, httpClient).run

    assert(
      statuses.length == 1,
      s"assumed one status response would be returned, got: ${statuses.length}"
    )

    assert(
      statuses.head.state == "failure",
      s"status set had wrong state, expected 'failure', got: ${statuses.head.state}"
    )

    httpClient.shutdownNow()
  }
}
