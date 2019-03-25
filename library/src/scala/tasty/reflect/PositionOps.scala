package scala.tasty.reflect

trait PositionOps extends Core {

  implicit class PositionAPI(pos: Position) {

    /** The start offset in the source file */
    def start: Int = kernel.Position_start(pos)

    /** The end offset in the source file */
    def end: Int = kernel.Position_end(pos)

    /** Does this position exist */
    def exists: Boolean = kernel.Position_exists(pos)

    /** Source file in which this position is located */
    def sourceFile: SourceFile = kernel.Position_sourceFile(pos)

    /** The start line in the source file */
    def startLine: Int = kernel.Position_startLine(pos)

    /** The end line in the source file */
    def endLine: Int = kernel.Position_endLine(pos)

    /** The start column in the source file */
    def startColumn: Int = kernel.Position_startColumn(pos)

    /** The end column in the source file */
    def endColumn: Int = kernel.Position_endColumn(pos)

    /** Source code within the position */
    def sourceCode: String = kernel.Position_sourceCode(pos)

  }

  implicit class SourceFileAPI(sourceFile: SourceFile) {

    /** Path to this source file */
    def jpath: java.nio.file.Path = kernel.SourceFile_jpath(sourceFile)

    /** Content of this source file */
    def content: String = kernel.SourceFile_content(sourceFile)

  }

}
