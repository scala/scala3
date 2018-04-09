package scala.tasty

trait Position {
  def start: Int
  def end: Int

  def sourceFile: java.nio.file.Path

  def startLine: Int
  def startColumn: Int
  def endLine: Int
  def endColumn: Int
}
