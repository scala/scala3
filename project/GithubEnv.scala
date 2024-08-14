import scala.util.Properties
import sbt.url
import java.net.URL


// https://docs.github.com/en/actions/writing-workflows/choosing-what-your-workflow-does/variables#default-environment-variables
object GithubEnv {
  lazy val repositoryVar: Option[(String, String)] = envVar("GITHUB_REPOSITORY")
  lazy val runIdVar: Option[(String, String)] = envVar("GITHUB_RUN_ID")
  lazy val shaVar: Option[(String, String)] = envVar("GITHUB_SHA")
  lazy val workflowVar: Option[(String, String)] = envVar("GITHUB_WORKFLOW")

  lazy val runUrl: Option[(String, URL)] =
    for {
      (_, repository) <- repositoryVar
      (_, runId) <- runIdVar
    } yield "GITHUB_RUN" -> url(s"https://github.com/$repository/actions/runs/$runId")
  lazy val treeUrl: Option[(String, URL)] =
    for {
      (_, repository) <- repositoryVar
      (_, sha) <- shaVar
    } yield "GITHUB_TREE" -> url(s"https://github.com/$repository/tree/$sha")


  def develocityValues: Seq[(String, String)] = repositoryVar.toSeq ++ shaVar ++ workflowVar
  def develocityLinks: Seq[(String, URL)] = runUrl.toSeq ++ treeUrl

  private def envVar(key: String): Option[(String, String)] =
    Properties.envOrNone(key).map(key -> _)
}
