package dotty.tools.dotc.tastyreflect

trait PositionOpsImpl extends scala.tasty.reflect.PositionOps with CoreImpl {

  def PositionDeco(pos: Position): PositionAPI = new PositionAPI {
    def start: Int = pos.start
    def end: Int = pos.end

    def exists: Boolean = pos.exists

    def sourceFile: java.nio.file.Path = pos.source.file.jpath

    def startLine: Int = pos.startLine
    def endLine: Int = pos.endLine

    def startColumn: Int = pos.startColumn
    def endColumn: Int = pos.endColumn

    def sourceCode: String = new String(pos.source.content(), pos.start, pos.end - pos.start)
  }
}
