import com.github.sbt.pullrequestvalidator.ValidatePullRequest
import com.github.sbt.pullrequestvalidator.ValidatePullRequest.PathGlobFilter
import sbt.Keys._
import sbt._

/** Maps each non-aggregator subproject to a PathGlobFilter so `validatePullRequest`
  * skips projects whose base directory is untouched by the PR diff.
  */
object Scala3ValidatePullRequest extends AutoPlugin {

  import ValidatePullRequest.autoImport._

  override lazy val trigger = allRequirements
  override lazy val requires = ValidatePullRequest

  /** Aggregator/root projects with no source directory of their own. */
  private val ignoredProjects: Set[String] = Set(
    "scala3-nonbootstrapped",
    "scala3-bootstrapped",
  )

  /** Matches top-level test-input directories that are read by the compiler /
    * scripted test suites but live outside any project's base directory.
    * Without this, a PR that only touches `tests/pos/i12345.scala` or `sbt-test/`
    * would match no project glob and `validatePullRequest` would skip the
    * compiler tests entirely. `tests/sjs-junit/` is excluded because it IS the
    * `sjsJUnitTests` project base and is handled by the per-project filter.
    */
  private val externalTestInputs: FileFilter = new FileFilter {
    override def accept(f: File): Boolean = {
      val p = f.getPath.replace(java.io.File.separatorChar, '/')
      (p.startsWith("tests/") && !p.startsWith("tests/sjs-junit/")) ||
      p.startsWith("sbt-test/")
    }
    override def toString: String = "Scala3ExternalTestInputsFilter"
  }

  override lazy val buildSettings = Seq(
    validatePullRequest / includeFilter := {
      val build = loadedBuild.value
      val rootDir = new File(build.root)

      build.allProjectRefs.iterator.collect {
        case (_, project) if !ignoredProjects.contains(project.id) =>
          val rel = rootDir.toPath
            .relativize(project.base.toPath)
            .toString
            .replace(java.io.File.separatorChar, '/')
          if (rel.isEmpty || rel == ".") FileFilter.nothing
          else PathGlobFilter(s"$rel/**")
      }.foldLeft(FileFilter.nothing: FileFilter)((acc, f) => acc || f)
    },
    validatePullRequestBuildAll / includeFilter := {
      (validatePullRequestBuildAll / includeFilter).value || externalTestInputs
    },
    prValidatorTargetBranch := sys.env.getOrElse("PR_TARGET_BRANCH", "origin/main"),
    prValidatorGithubRepository := Some("scala/scala3"),
  )
}
