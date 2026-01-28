package scala.quoted.meta

trait Position private[meta] {

  /** The start offset of the position in the source code */
  def start: Int

  /** The end offset of the position in the source code */
  def end: Int

  /** The source file name */
  def sourceFile: SourceFile

  /** The line number of the start of the position */
  def startLine: Int

  /** The line number of the end of the position */
  def endLine: Int

  /** The column number of the start of the position */
  def startColumn: Int

  /** The column number of the end of the position */
  def endColumn: Int

}
object Position {

  def api(using meta: Meta): Meta.PositionAPI = meta.internal.position
  given Meta => Conversion[Position.type, Meta.PositionAPI] = _.api

}
