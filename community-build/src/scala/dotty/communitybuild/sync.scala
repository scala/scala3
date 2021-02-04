package dotty.communitybuild

import ammonite.ops._
import org.eclipse.egit.github.core
import org.eclipse.egit.github.core.{PullRequest, PullRequestMarker, RepositoryId}
import org.eclipse.egit.github.core.client.GitHubClient
import org.eclipse.egit.github.core.service.{PullRequestService, RepositoryService}
import sys.process._
import cats.effect.{Blocker, ContextShift, IO}
import org.http4s.client.JavaNetClientBuilder
import java.util.concurrent._
import scala.concurrent.ExecutionContext.global
import org.http4s.client.middleware.FollowRedirect
import util.Try
import util.Success
import util.Failure

private case class Inputs(
  login: String,
  token: String,
  name: String,
  email: String,
  prBranch: String,
  prTitle: String,
  diffUrl: String)

def syncAll(): Unit =
  implicit val cs: ContextShift[IO] = IO.contextShift(global)
  val blockingPool = Executors.newSingleThreadExecutor()
  val blocker = Blocker.liftExecutorService(blockingPool)
  val httpClient = FollowRedirect(1)(JavaNetClientBuilder[IO](blocker).create)

  getInputs().foreach(inputs => {
    val gitHub = new GitHub(inputs.token)
    val diff = httpClient.expect[String](inputs.diffUrl).unsafeRunSync()
    getSubmoduleDiffs(diff)
      .filter(submoduleDiff => projectMap.get(submoduleDiff.repo).map(project => project.sync).getOrElse(false))
      .foreach(submoduleDiff => syncSubmodule(gitHub, inputs, submoduleDiff))
  })

private def getInputs(): Option[Inputs] =
  for {
    login <- sys.env.get("LOGIN")
    token <- sys.env.get("TOKEN")
    name <- sys.env.get("NAME")
    email <- sys.env.get("EMAIL")
    prBranch <- sys.env.get("PR_BRANCH")
    prTitle <- sys.env.get("PR_TITLE")
    diffUrl <- sys.env.get("DIFF_URL")
  } yield Inputs(
    login,
    token,
    name,
    email,
    prBranch,
    prTitle,
    diffUrl)


def syncSubmodule(gitHub: GitHub, inputs: Inputs, submoduleDiff: SubmoduleDiff): Unit = {
  println(s"Syncing: ${submoduleDiff.repo}")

  val wd = pwd / up
  val remoteUrl = git.remoteUrl(wd / submoduleDiff.repoDir)
  val (owner, repoName) = getRepoFullName(remoteUrl)
  val repo = gitHub.repos.get(owner, repoName)
  val parent = repo.getParent
  git.configure(wd / submoduleDiff.repoDir, repo.getOwner.getLogin, repo.getName, inputs.email, inputs.name, git.Auth(inputs.login, inputs.token))
  git.checkout(wd / submoduleDiff.repoDir, inputs.prBranch, Some(submoduleDiff.currentHash))
  git.pushForce(wd / submoduleDiff.repoDir, inputs.prBranch)
  gitHub.pulls.create(
    parent.getOwner.getLogin,
    parent.getName,
    parent.getMasterBranch,
    s"${repo.getOwner.getLogin}:${inputs.prBranch}",
    inputs.prTitle)
}

private def getRepoFullName(remoteUrl: String): (String, String) =
  val parts = remoteUrl.trim().split("/")
  val owner = parts(parts.length - 2)
  val repoWithExt = parts.last
  val repo =
    if (repoWithExt.contains(".git")) repoWithExt.substring(0, repoWithExt.lastIndexOf(".git"))
    else repoWithExt
  (owner, repo)

case class SubmoduleDiff(
    repoDir: RelPath,
    repo: String,
    previousHash: String,
    currentHash: String)

def getSubmoduleDiffs(diff: String): List[SubmoduleDiff] =
  getDiffs(diff).filter(isSubmoduleDiff).map(getSubmoduleDiff)

private def getDiffs(diff: String): List[List[String]] =
  diff
    .split("\n")
    .map(_.trim)
    .filter(_.nonEmpty)
    .foldLeft(List(List[String]()))((previous, current) => {
      if (current.startsWith("diff"))
        previous :+ List(current)
      else
        previous.updated(previous.length - 1, previous.last :+ current)
    })
    .filter(_.nonEmpty)

private def isSubmoduleDiff(diff: List[String]): Boolean =
  diff.length == 7 && diff(5).startsWith("-Subproject") && diff(6).startsWith("+Subproject")

private def getSubmoduleDiff(diff: List[String]): SubmoduleDiff =
  val dir = RelPath(diff.head.trim.substring(11).split(" ")(0).substring(2))
  val name = dir.baseName
  val previous = diff(5).trim.substring(19)
  val current = diff(6).trim.substring(19)
  SubmoduleDiff(dir, name, previous, current)

class GitHub(private val token: String):
  private val client = new GitHubClient().setOAuth2Token(token)

  lazy val repos: Repositories = new Repositories()
  lazy val pulls: PullRequests = new PullRequests()

  class Repositories:
    private val repositoryService = new RepositoryService(client)

    def get(owner: String, repo: String): core.Repository =
      repositoryService.getRepository(owner, repo)
  end Repositories

  class PullRequests:
    private val pullRequestService = new PullRequestService(client)

    def create(owner: String, repo: String, base: String, head: String, title: String): Unit =
      Try {
        pullRequestService.createPullRequest(
          new RepositoryId(owner, repo),
          new PullRequest()
            .setBase(new PullRequestMarker().setLabel(base))
            .setHead(new PullRequestMarker().setLabel(head))
            .setTitle(title))
      } match {
        case Failure(e: Exception) => println(e.getMessage)
        case _ =>
      }
  end PullRequests
end GitHub

object git:
  case class Auth(name: String, token: String)

  def configure(repoDir: Path, owner: String, repo: String, email: String, name: String, auth: Auth): Unit =
    process.exec(s"git config --global user.name $name", repoDir)
    process.exec(s"git config --global user.email $email", repoDir)
    process.exec(s"git remote set-url origin https://${auth.name}:${auth.token}@github.com/$owner/$repo.git", repoDir)

  def remoteUrl(repoDir: Path): String =
    process.exec("git remote get-url origin", repoDir)

  def checkout(repoDir: Path, branchName: String, hash: Option[String] = None): Unit =
    process.exec(s"git checkout -b $branchName ${hash.getOrElse("")}", repoDir)

  def pushForce(repoDir: Path, branchName: String): Unit =
    process.exec(s"git push -f origin $branchName", repoDir)
end git

object process:
  def exec(cmd: String, location: Path): String =
    println(s"Executing $cmd at $location")
    sys.process.Process(cmd, location.toIO).!!.trim
end process
