package dotty.communitybuild

import java.nio.file._
import java.io.{PrintWriter, File}
import java.nio.charset.StandardCharsets.UTF_8

object CommunityBuildRunner:

  /** Depending on the mode of operation, either
   *  runs the test or updates the project. Updating
   *  means that all the dependencies are fetched but
   *  minimal other extra other work is done. Updating
   *  is necessary since we run tests each time on a fresh
   *  Docker container. We run the update on Docker container
   *  creation time to create the cache of the dependencies
   *  and avoid network overhead. See https://github.com/lampepfl/dotty-drone
   *  for more infrastructural details.
   */
  extension (self: CommunityProject) def run()(using suite: CommunityBuildRunner): Unit =
    if self.requiresExperimental && !self.compilerSupportExperimental then
      log(s"Skipping ${self.project} - it needs experimental features unsupported in this build.")
      return
    self.dependencies().foreach(_.publish())
    self.testOnlyDependencies().foreach(_.publish())
    suite.runProject(self)

trait CommunityBuildRunner:

  /** fails the current operation, can be specialised in a concrete Runner
   *  - overridden in `CommunityBuildTest`
   */
  def failWith(msg: String): Nothing = throw IllegalStateException(msg)

  /** Build the given project with the published local compiler and sbt plugin.
   *
   *  This test reads the compiler version from community-build/dotty-bootstrapped.version
   *  and expects community-build/sbt-dotty-sbt to set the compiler plugin.
   *
   *  @param project    The project name, should be a git submodule in community-build/
   *  @param command    The binary file of the program used to test the project – usually
   *                    a build tool like SBT or Mill
   *  @param arguments  Arguments to pass to the testing program
   */
  def runProject(projectDef: CommunityProject): Unit =
    val project = projectDef.project
    val command = projectDef.binaryName
    val arguments = projectDef.buildCommands
    val compilerVersion = projectDef.compilerVersion

    @annotation.tailrec
    def execTimes(task: () => Int, timesToRerun: Int): Boolean =
      val exitCode = task()
      if exitCode == 0
      then true
      else if timesToRerun == 0
        then false
        else
          log(s"Rerunning tests in $project because of a previous run failure.")
          execTimes(task, timesToRerun - 1)

    log(s"Building $project with dotty-bootstrapped $compilerVersion...")

    val projectDir = communitybuildDir.resolve("community-projects").resolve(project)

    if !Files.exists(projectDir.resolve(".git")) then
      failWith(s"""
        |
        |Missing $project submodule. You can initialize this module using
        |
        |    git submodule update --init community-build/community-projects/$project
        |
        |""".stripMargin)

    val testsCompletedSuccessfully = execTimes(projectDef.build, 3)

    if !testsCompletedSuccessfully then
      failWith(s"""
          |
          |$command exited with an error code. To reproduce without JUnit, use:
          |
          |    sbt community-build/prepareCommunityBuild
          |    cd community-build/community-projects/$project
          |    $command ${arguments.init.mkString(" ")} "${arguments.last}"
          |
          |For a faster feedback loop on SBT projects, one can try to extract a direct call to dotc
          |using the sbt export command. For instance, for scalacheck, use
          |    sbt export jvm/test:compileIncremental
          |
          |""".stripMargin)
  end runProject

end CommunityBuildRunner
