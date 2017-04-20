package dotty.tools.bot
package model

object Github {
  case class PullRequest(
    url: String,
    id: Long,
    commits_url: String
  )

  case class Issue(
    action: String, // "opened", "reopened", "closed", "synchronize"
    number: Int,
    pull_request: Option[PullRequest]
  )

  case class CommitInfo(
    message: String
  )

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

  // TODO: Can we get a `Commit` from `StatusResponse`? 
  case class StatusResponse(
    url: String,
    id: Long,
    state: String
  )

  case class Comment(user: Author)
}
