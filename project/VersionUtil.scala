import org.eclipse.jgit.storage.file.FileRepositoryBuilder
import org.eclipse.jgit.lib.{Constants, ObjectId, Ref, Repository}
import org.eclipse.jgit.revwalk.{RevCommit, RevWalk}

import java.text.SimpleDateFormat
import java.util.{Date, TimeZone}

object VersionUtil {

  // Adapted from sbt-git
  private class JGit(repo: Repository) {
    def headCommit: ObjectId =
      repo.exactRef(Constants.HEAD).getObjectId

    def headCommitSha: String = headCommit.name

    def headCommitDate: Date = {
      val walk = new RevWalk(repo)
      val commit = walk.parseCommit(headCommit)
      val seconds = commit.getCommitTime.toLong
      val millis = seconds * 1000L
      new Date(millis)
    }
  }

  private lazy val git = {
    val repo = new FileRepositoryBuilder()
      .setMustExist(true)
      .findGitDir()
      .build()
    new JGit(repo)
  }

  /** Full SHA hash of the current commit */
  def gitHashFull: String = git.headCommitSha

  /** Seven letters of the SHA hash is considered enough to uniquely identify a
   *  commit, albeit extremely large projects - such as the Linux kernel - need
   *  more letters to stay unique
   */
  def gitHash: String = git.headCommitSha.substring(0, 7)
  def commitDate: String = {
    val format = new SimpleDateFormat("yyyyMMdd")
    format.setTimeZone(TimeZone.getTimeZone("UTC"))
    format.format(git.headCommitDate)
  }
}
