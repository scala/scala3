package dotty.tools.dotc.quoted

class ToolboxSettings private (val outDir: Option[String], val rawTree: Boolean, val compilerArgs: List[String])

object ToolboxSettings {

  implicit def default: ToolboxSettings = make()

  /** Make toolbox settings
    *  @param outDir Output directory for the compiled quote. If set to None the output will be in memory
    *  @param color Print output with colors
    *  @param rawTree Do not remove quote tree artifacts
    *  @param compilerArgs Compiler arguments. Use only if you know what you are doing.
    */
  def make(
            color: Boolean = false,
            rawTree: Boolean = false,
            outDir: Option[String] = None,
            compilerArgs: List[String] = Nil
          ): ToolboxSettings =
    new ToolboxSettings(outDir, rawTree, compilerArgs)
}
