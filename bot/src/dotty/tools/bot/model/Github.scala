package dotty.tools.bot
package model

object Github {
  case class PullRequest(
    url: String,
    id: Long,
    commits_url: String
  )

  case class Issue(
    number: Int,
    pull_request: Option[PullRequest]
  )
}
