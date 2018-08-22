package dotty.tools.dotc.tastyreflect

trait PositionOpsImpl extends scala.tasty.reflect.PositionOps with TastyCoreImpl {

  def PositionDeco(pos: Position): PositionAPI = new PositionAPI {
    def start: Int = pos.start
    def end: Int = pos.end

    def sourceFile: java.nio.file.Path = pos.source.file.jpath

    def startLine: Int = pos.startLine
    def endLine: Int = pos.endLine

    def startColumn: Int = pos.startColumn
    def endColumn: Int = pos.endColumn
  }

}
