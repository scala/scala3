package dotty.tools.bot
package model

object Github {
  case class Issue(
    action: Option[String], // "opened", "reopened", "closed", "synchronize"
    number: Int,
    pull_request: Option[PullRequest]
  )

  case class PullRequest(url: String, id: Option[Long], commits_url: Option[String])

  case class CommitInfo(message: String)

  case class Commit(
    sha: String,
    author: Author,
    committer: Author,
    commit: CommitInfo
  )

  case class Author(
    login: Option[String]
  )

  case class Status(
    state: String,
    target_url: String,
    description: String,
    context: String = "CLA"
  )

  case class StatusResponse(
    url: String,
    id: Long,
    state: String,
    context: String,
    target_url: String
  ) {
    def sha: String = url.split('/').last
  }

  /** A PR review */
  case class Review(body: String, event: String)

  object Review {
    def approve(body: String) = Review(body, "APPROVE")
    def requestChanges(body: String) = Review(body, "REQUEST_CHANGES")
    def comment(body: String) = Review(body, "COMMENT")
  }

  case class ReviewResponse(body: String, state: String, id: Long)

  case class IssueComment(action: String, issue: Issue, comment: Comment)

  case class Comment(user: Author, body: String)
}
