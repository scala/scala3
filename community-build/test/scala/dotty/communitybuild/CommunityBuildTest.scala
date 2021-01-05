package dotty.communitybuild

import java.nio.file._
import java.io.{PrintWriter, File}
import java.nio.charset.StandardCharsets.UTF_8
import org.junit.{Ignore, Test}
import org.junit.Assert.{assertEquals, fail}
import org.junit.experimental.categories.Category

abstract class CommunityBuildTest:
  given CommunityBuildTest = this

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
  extension (self: CommunityProject) def run()(using suite: CommunityBuildTest) =
    self.dependencies.foreach(_.publish())
    suite.test(self.project, self.binaryName, self.runCommandsArgs :+ self.testCommand)

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
  def test(project: String, command: String, arguments: Seq[String]): Unit = {
    @annotation.tailrec
    def execTimes(task: => Int, timesToRerun: Int): Boolean =
      val exitCode = task
      if exitCode == 0
      then true
      else if timesToRerun == 0
        then false
        else
          log(s"Rerunning tests in $project because of a previous run failure.")
          execTimes(task, timesToRerun - 1)

    log(s"Building $project with dotty-bootstrapped $compilerVersion...")

    val projectDir = communitybuildDir.resolve("community-projects").resolve(project)

    if (!Files.exists(projectDir.resolve(".git"))) {
      fail(s"""
        |
        |Missing $project submodule. You can initialize this module using
        |
        |    git submodule update --init community-build/community-projects/$project
        |
        |""".stripMargin)
    }

    val testsCompletedSuccessfully = execTimes(exec(projectDir, command, arguments: _*), 3)

    if (!testsCompletedSuccessfully) {
      fail(s"""
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
    }
  }
end CommunityBuildTest

@Category(Array(classOf[TestCategory]))
class CommunityBuildTestA extends CommunityBuildTest:
  @Test def endpoints4s = projects.endpoints4s.run()
  @Test def fansi = projects.fansi.run()
  @Test def fastparse = projects.fastparse.run()
  @Test def geny = projects.geny.run()
  @Test def izumiReflect = projects.izumiReflect.run()
  @Test def oslib = projects.oslib.run()
  // @Test def oslibWatch = projects.oslibWatch.run()
  @Test def pprint = projects.pprint.run()
  @Test def requests = projects.requests.run()
  @Test def scalacheck = projects.scalacheck.run()
  @Test def scalaCollectionCompat = projects.scalaCollectionCompat.run()
  @Test def scalaParallelCollections = projects.scalaParallelCollections.run()
  @Test def scalatest = projects.scalatest.run()
  @Test def scalatestplusScalacheck = projects.scalatestplusScalacheck.run()
  @Test def scalaz = projects.scalaz.run()
  @Test def sourcecode = projects.sourcecode.run()
  @Test def stdLib213 = projects.stdLib213.run()
  @Test def ujson = projects.ujson.run()
  @Test def upickle = projects.upickle.run()
  @Test def utest = projects.utest.run()
  @Test def zio = projects.zio.run()

  // 'scala-stm' and 'Sciss/Lucre':
  // @Test def scissEqual      = projects.scissEqual     .run()
  // @Test def scissFingerTree = projects.scissFingerTree.run()
  // @Test def scissLog        = projects.scissLog       .run()
  // @Test def scissModel      = projects.scissModel     .run()
  // @Test def scissNumbers    = projects.scissNumbers   .run()
  // @Test def scissSerial     = projects.scissSerial    .run()
  // @Test def scissAsyncFile  = projects.scissAsyncFile .run()
  // @Test def scissSpan       = projects.scissSpan      .run()
  @Test def scalaSTM        = projects.scalaSTM       .run()
  // @Test def scissLucre      = projects.scissLucre     .run()

end CommunityBuildTestA

@Category(Array(classOf[TestCategory]))
class CommunityBuildTestB extends CommunityBuildTest:
  @Test def algebra = projects.algebra.run()
  @Test def betterfiles = projects.betterfiles.run()
  @Test def cats = projects.cats.run()
  @Test def catsEffect2 = projects.catsEffect2.run()
  @Test def catsEffect3 = projects.catsEffect3.run()
  @Test def catsMtl = projects.catsMtl.run()
  @Test def coop = projects.coop.run()
  @Test def discipline = projects.discipline.run()
  @Test def disciplineMunit = projects.disciplineMunit.run()
  @Test def disciplineSpecs2 = projects.disciplineSpecs2.run()
  // Temporarily disabled until problem discovered in comments to #9449 is fixed
  // @Test def dottyCpsAsync = projects.dottyCpsAsync.run()
  @Test def effpi = projects.effpi.run()
  @Test def intent = projects.intent.run()
  @Test def minitest = projects.minitest.run()
  @Test def munit = projects.munit.run()
  @Test def scodec = projects.scodec.run()
  @Test def scodecBits = projects.scodecBits.run()
  @Test def scalap = projects.scalap.run()
  @Test def scalaParserCombinators = projects.scalaParserCombinators.run()
  @Test def ScalaPB = projects.ScalaPB.run()
  @Test def scalaXml = projects.scalaXml.run()
  @Test def scas = projects.scas.run()
  @Test def sconfig = projects.sconfig.run()
  @Test def shapeless = projects.shapeless.run()
  @Test def simulacrumScalafixAnnotations = projects.simulacrumScalafixAnnotations.run()
  @Test def verify = projects.verify.run()
  @Test def xmlInterpolator = projects.xmlInterpolator.run()

end CommunityBuildTestB

class TestCategory
