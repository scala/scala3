package mill.scala3buildhelper

import mill.api.BuildCtx
import mill.scalalib.*
import mill.constants.OutFiles.OutFiles

/**
 * Helper module to access private[mill] methods
 */
trait CompileClassesPathHelper extends ScalaModule {

  /**
   * Actual Mill output directory
   *
   * @param taskDest The dest directory of a task of this build (can be any task)
   */
  private def outDir(taskDest: os.Path): os.Path = {
    val bspOut = BuildCtx.workspaceRoot / os.SubPath(OutFiles.bspOut)
    if (taskDest.startsWith(bspOut)) bspOut
    else BuildCtx.workspaceRoot / os.SubPath(OutFiles.out)
  }

  /**
   * Computes the expected output directory of the compile task
   *
   * @param taskDest The dest directory of a task of this build (can be any task)
   */
  def compileClassesPath0(taskDest: os.Path) = compileClassesPath.resolve(outDir(taskDest))

  /**
   * Computes the expected output directory of the jar task
   *
   * @param taskDest The dest directory of a task of this build (can be any task)
   */
  def jarPath(taskDest: os.Path) =
    resolveRelativeToOut(jar, identity).resolve(outDir(taskDest))
}
