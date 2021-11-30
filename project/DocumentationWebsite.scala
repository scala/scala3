import java.io.File
import java.nio.file.Paths
import sbt._
import Build._

object DocumentationWebsite {
  def generateStaticAssets(
    contributorsFile: File,
    mainFile: File,
    cssContentContributorsSourceBaseFile: File,
    cssSourceFileBase: File,
    baseDest: File
  ): Seq[File] = {


    val contributorsTestcasesDestinationFile = Paths.get("scaladoc-testcases", "docs", "js", "contributors.js").toFile
    val contributorsDestinationFile = Paths.get("docs-for-dotty-page", "js", "contributors.js").toFile
    sbt.IO.copyFile(contributorsFile, contributorsTestcasesDestinationFile)
    sbt.IO.copyFile(contributorsFile, contributorsDestinationFile)

    val mainDestinationFile = baseDest / "dotty_res" / "scripts" / "scaladoc-scalajs.js"
    sbt.IO.copyFile(mainFile, mainDestinationFile)

    val cssCodeSnippetsDesitnationFile = baseDest / "dotty_res" / "styles" / "code-snippets.css"
    val cssCodeSnippetsSourceFile = cssSourceFileBase / "code-snippets.css"
    sbt.IO.copyFile(cssCodeSnippetsSourceFile, cssCodeSnippetsDesitnationFile)

    val cssContentContributorsTestcasesDesitnationFile = Paths.get("docs-for-dotty-page", "css", "content-contributors.css").toFile
    val cssContentContributorsDesitnationFile = Paths.get("scaladoc-testcases", "docs", "css", "content-contributors.css").toFile
    val cssContentContributorsSourceFile = cssContentContributorsSourceBaseFile / "content-contributors.css"
    sbt.IO.copyFile(cssContentContributorsSourceFile, cssContentContributorsTestcasesDesitnationFile)
    sbt.IO.copyFile(cssContentContributorsSourceFile, cssContentContributorsDesitnationFile)

    val dests = Seq("searchbar.css", "social-links.css", "ux.css", "versions-dropdown.css").map { file =>
      val cssDesitnationFile = baseDest / "dotty_res" / "styles" / file
      val cssSourceFile = cssSourceFileBase / file
      sbt.IO.copyFile(cssSourceFile, cssDesitnationFile)
      cssDesitnationFile
    }

    import _root_.scala.sys.process._
    import _root_.scala.concurrent._
    import _root_.scala.concurrent.duration.Duration
    import ExecutionContext.Implicits.global
    val inkuireVersion = "1.0.0-M3"
    val inkuireLink = s"https://github.com/VirtusLab/Inkuire/releases/download/$inkuireVersion/inkuire.js"
    val inkuireDestinationFile = baseDest / "dotty_res" / "scripts" / "inkuire.js"
    sbt.IO.touch(inkuireDestinationFile)

    def tryFetch(retries: Int, timeout: Duration): Unit = {
      val downloadProcess = (new java.net.URL(inkuireLink) #> inkuireDestinationFile).run()
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
      cssContentContributorsDesitnationFile,
      cssCodeSnippetsDesitnationFile,
    ) ++ dests
  }
}


