package scala.tasty

trait Position {
  def firstOffset: Int
  def lastOffset: Int

  def sourceFile: String

  def startLine: Int
  def startColumn: Int
  def endLine: Int
  def endColumn: Int
}
