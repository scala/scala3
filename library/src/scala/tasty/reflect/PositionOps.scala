package scala.tasty.reflect

trait PositionOps extends Core {

  given PositionOps: (pos: Position) {

    /** The start offset in the source file */
    def start: Int = internal.Position_start(pos)

    /** The end offset in the source file */
    def end: Int = internal.Position_end(pos)

    /** Does this position exist */
    def exists: Boolean = internal.Position_exists(pos)

    /** Source file in which this position is located */
    def sourceFile: SourceFile = internal.Position_sourceFile(pos)

    /** The start line in the source file */
    def startLine: Int = internal.Position_startLine(pos)

    /** The end line in the source file */
    def endLine: Int = internal.Position_endLine(pos)

    /** The start column in the source file */
    def startColumn: Int = internal.Position_startColumn(pos)

    /** The end column in the source file */
    def endColumn: Int = internal.Position_endColumn(pos)

    /** Source code within the position */
    def sourceCode: String = internal.Position_sourceCode(pos)

  }

  given SourceFileOps: (sourceFile: SourceFile) {

    /** Path to this source file */
    def jpath: java.nio.file.Path = internal.SourceFile_jpath(sourceFile)

    /** Content of this source file */
    def content: String = internal.SourceFile_content(sourceFile)

  }

}
