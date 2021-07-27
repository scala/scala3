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
  extension (self: CommunityProject) def run()(using suite: CommunityBuildTest): Unit =
    if self.requiresExperimental && !compilerSupportExperimental then
      println(
        s"Skipping ${self.project} - it needs experimental features unsupported in this build."
      )
      return
    self.dependencies.foreach(_.publish())
    self.testOnlyDependencies().foreach(_.publish())
    suite.test(self)

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
  def test(projectDef: CommunityProject): Unit = {
    val project = projectDef.project
    val command = projectDef.binaryName
    val arguments = projectDef.buildCommands

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

    if (!Files.exists(projectDir.resolve(".git"))) {
      fail(s"""
        |
        |Missing $project submodule. You can initialize this module using
        |
        |    git submodule update --init community-build/community-projects/$project
        |
        |""".stripMargin)
    }

    val testsCompletedSuccessfully = execTimes(projectDef.build, 3)

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

class TestCategory

@Category(Array(classOf[TestCategory]))
class CommunityBuildTestA extends CommunityBuildTest:
  @Test def izumiReflect = projects.izumiReflect.run()
  @Test def scalaSTM = projects.scalaSTM.run()
  @Test def scalatest = projects.scalatest.run()
  @Test def scalatestplusTestNG = projects.scalatestplusTestNG.run()
  // 'Sciss/Lucre' dependencies:
  // @Test def scissEqual      = projects.scissEqual     .run()
  // @Test def scissFingerTree = projects.scissFingerTree.run()
  // @Test def scissLog        = projects.scissLog       .run()
  // @Test def scissModel      = projects.scissModel     .run()
  // @Test def scissNumbers    = projects.scissNumbers   .run()
  // @Test def scissSerial     = projects.scissSerial    .run()
  // @Test def scissAsyncFile  = projects.scissAsyncFile .run()
  // @Test def scissSpan       = projects.scissSpan      .run()
  @Test def scissLucre = projects.scissLucre.run()
  @Test def zio = projects.zio.run()
end CommunityBuildTestA

@Category(Array(classOf[TestCategory]))
class CommunityBuildTestB extends CommunityBuildTest:
  @Test def cats = projects.cats.run()
  @Test def catsEffect3 = projects.catsEffect3.run()
  @Test def catsMtl = projects.catsMtl.run()
  @Test def coop = projects.coop.run()
  @Test def discipline = projects.discipline.run()
  @Test def disciplineMunit = projects.disciplineMunit.run()
  @Test def disciplineSpecs2 = projects.disciplineSpecs2.run()
  @Test def fs2 = projects.fs2.run()
  @Test def munit = projects.munit.run()
  @Test def munitCatsEffect = projects.munitCatsEffect.run()
  @Test def perspective = projects.perspective.run()
  @Test def scalacheckEffect = projects.scalacheckEffect.run()
  @Test def scodec = projects.scodec.run()
  @Test def scodecBits = projects.scodecBits.run()
  @Test def monocle = projects.monocle.run()
  @Test def simulacrumScalafixAnnotations = projects.simulacrumScalafixAnnotations.run()
end CommunityBuildTestB

@Category(Array(classOf[TestCategory]))
class CommunityBuildTestC extends CommunityBuildTest:
  @Test def akka = projects.akka.run()
  @Test def algebra = projects.algebra.run()
  @Test def betterfiles = projects.betterfiles.run()
  @Test def cask = projects.cask.run()
  // Temporarily disabled until problem discovered in comments to #9449 is fixed
  // @Test def dottyCpsAsync = projects.dottyCpsAsync.run()
  @Test def effpi = projects.effpi.run()
  @Test def endpoints4s = projects.endpoints4s.run()
  @Test def fansi = projects.fansi.run()
  @Test def fastparse = projects.fastparse.run()
  @Test def geny = projects.geny.run()
  @Test def intent = projects.intent.run()
  @Test def jacksonModuleScala = projects.jacksonModuleScala.run()
  @Test def libretto = projects.libretto.run()
  @Test def minitest = projects.minitest.run()
  @Test def onnxScala = projects.onnxScala.run()
  @Test def oslib = projects.oslib.run()
  // @Test def oslibWatch = projects.oslibWatch.run()
  @Test def playJson = projects.playJson.run()
  @Test def pprint = projects.pprint.run()
  @Test def protoquill = projects.protoquill.run()
  @Test def requests = projects.requests.run()
  @Test def scalacheck = projects.scalacheck.run()
  @Test def scalaCollectionCompat = projects.scalaCollectionCompat.run()
  @Test def scalaJava8Compat = projects.scalaJava8Compat.run()
  @Test def scalap = projects.scalap.run()
  @Test def scalaParallelCollections = projects.scalaParallelCollections.run()
  @Test def scalaParserCombinators = projects.scalaParserCombinators.run()
  @Test def scalaPB = projects.scalaPB.run()
  @Test def scalatestplusScalacheck = projects.scalatestplusScalacheck.run()
  @Test def scalaXml = projects.scalaXml.run()
  @Test def scalaz = projects.scalaz.run()
  @Test def scas = projects.scas.run()
  @Test def sconfig = projects.sconfig.run()
  @Test def shapeless = projects.shapeless.run()
  @Test def sourcecode = projects.sourcecode.run()
  @Test def stdLib213 = projects.stdLib213.run()
  @Test def ujson = projects.ujson.run()
  @Test def upickle = projects.upickle.run()
  @Test def utest = projects.utest.run()
  @Test def verify = projects.verify.run()
  @Test def xmlInterpolator = projects.xmlInterpolator.run()
end CommunityBuildTestC
