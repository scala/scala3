import java.io.File
import java.net.URI
import java.nio.file.Paths
import sbt._

object DocumentationWebsite {
  def generateStaticAssets(
    contributorsFile: File,
    mainFile: File,
    cssContentContributorsSourceBaseFile: File,
    cssSourceFileBase: File,
    baseDest: File
  ): Seq[File] = {


    val contributorsTestcasesDestinationFile = Paths.get("scaladoc-testcases", "docs", "_assets", "js", "contributors.js").toFile
    val contributorsDestinationFile = baseDest / "dotty_res" / "scripts" / "contributors.js"
    sbt.IO.copyFile(contributorsFile, contributorsTestcasesDestinationFile)
    sbt.IO.copyFile(contributorsFile, contributorsDestinationFile)

    val mainDestinationFile = baseDest / "dotty_res" / "scripts" / "scaladoc-scalajs.js"
    sbt.IO.copyFile(mainFile, mainDestinationFile)

    val cssContentContributorsTestcasesDestinationFile = Paths.get("scaladoc-testcases", "docs", "_assets", "css", "content-contributors.css").toFile
    val cssContentContributorsDestinationFile = baseDest / "dotty_res" / "styles" / "content-contributors.css"
    val cssContentContributorsSourceFile = cssContentContributorsSourceBaseFile / "content-contributors.css"
    sbt.IO.copyFile(cssContentContributorsSourceFile, cssContentContributorsTestcasesDestinationFile)
    sbt.IO.copyFile(cssContentContributorsSourceFile, cssContentContributorsDestinationFile)

    val dests = Seq("searchbar.css", "social-links.css", "versions-dropdown.css").map { file =>
      val cssDestinationFile = baseDest / "dotty_res" / "styles" / file
      val cssSourceFile = cssSourceFileBase / file
      sbt.IO.copyFile(cssSourceFile, cssDestinationFile)
      cssDestinationFile
    }

    import _root_.scala.sys.process._
    import _root_.scala.concurrent._
    import _root_.scala.concurrent.duration.Duration
    import ExecutionContext.Implicits.global
    val inkuireVersion = "v1.0.0-M9"
    val inkuireLink = s"https://github.com/VirtusLab/Inkuire/releases/download/$inkuireVersion/inkuire.js"
    val inkuireDestinationFile = baseDest / "dotty_res" / "scripts" / "inkuire.js"
    sbt.IO.touch(inkuireDestinationFile)

    def tryFetch(retries: Int, timeout: Duration): Unit = {
      val downloadProcess = (new URI(inkuireLink).toURL #> inkuireDestinationFile).run()
      val result: Future[Int] = Future(blocking(downloadProcess.exitValue()))
      try {
        Await.result(result, timeout) match {
          case 0 =>
          case res if retries > 0 =>
            println(s"Failed to fetch inkuire.js from $inkuireLink: Error code $res. $retries retries left")
            tryFetch(retries - 1, timeout)
          case res => throw new MessageOnlyException(s"Failed to fetch inkuire.js from $inkuireLink: Error code $res")
        }
      } catch {
        case e: TimeoutException =>
          downloadProcess.destroy()
          if (retries > 0) {
            println(s"Failed to fetch inkuire.js from $inkuireLink: Download timeout. $retries retries left")
            tryFetch(retries - 1, timeout)
          }
          else {
            throw new MessageOnlyException(s"Failed to fetch inkuire.js from $inkuireLink: Download timeout")
          }
      }
    }

    tryFetch(5, Duration(60, "s"))
    Seq(
      inkuireDestinationFile,
      mainDestinationFile,
      contributorsDestinationFile,
      cssContentContributorsDestinationFile,
    ) ++ dests
  }
}


