package dotty.tools.bot

import org.junit.Assert._
import org.junit.Test

import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.parser.decode

import model.Github._
import org.http4s.client.blaze._
import scalaz.concurrent.Task

class PRServiceTests extends PullRequestService {
  val user = sys.env("USER")
  val token = sys.env("TOKEN")

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
}
