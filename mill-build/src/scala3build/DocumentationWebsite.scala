package scala3build

import coursier.cache.FileCache
import coursier.util.Artifact
import mill.api.BuildCtx

object DocumentationWebsite {

  def generateScaladocTestCasesAssets(
    workspaceRoot: os.Path,
    contributorsFile: os.Path,
    cssContentContributorsSourceBaseFile: os.Path
  ): Unit = {
    val assetsDir = workspaceRoot / "scaladoc-testcases/docs/_assets"
    os.copy(
      contributorsFile,
      assetsDir / "js/contributors.js"
    )
    os.copy(
      cssContentContributorsSourceBaseFile / "content-contributors.css",
      assetsDir / "css/content-contributors.css"
    )
  }

  def generateStaticAssets(
    workspaceRoot: os.Path,
    contributorsFile: os.Path,
    mainFile: os.Path,
    cssContentContributorsSourceBaseFile: os.Path,
    cssSourceFileBase: os.Path,
    destResourceDir: os.Path
  ): Unit = {

    val scriptsDir = destResourceDir / "dotty_res/scripts"
    val stylesDir = destResourceDir / "dotty_res/styles"

    os.makeDir.all(scriptsDir)
    os.makeDir.all(stylesDir)

    os.copy(contributorsFile, scriptsDir / "contributors.js")
    os.copy(mainFile, scriptsDir / "scaladoc-scalajs.js")

    for {
      cssDir <- Seq(cssSourceFileBase, cssContentContributorsSourceBaseFile)
      cssFile <- os.list(cssDir)
      if os.isFile(cssFile) && cssFile.last.endsWith(".css")
    }
      os.copy(cssFile, stylesDir / cssFile.last)

    val cache = FileCache()
    val inkuireJs = cache
      .file(Artifact(s"https://github.com/VirtusLab/Inkuire/releases/download/${Versions.inkuireVersion}/inkuire.js"))
      .run.unsafeRun(true)(using cache.ec)
      .fold(throw _, os.Path(_))
    os.copy(inkuireJs, scriptsDir / "inkuire.js")
  }
}


