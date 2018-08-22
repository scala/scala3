package scala.tasty.reflect

trait PositionOps extends TastyCore {

  trait PositionAPI {
    def start: Int
    def end: Int

    def sourceFile: java.nio.file.Path

    def startLine: Int
    def startColumn: Int
    def endLine: Int
    def endColumn: Int
  }
  implicit def PositionDeco(pos: Position): PositionAPI

}
