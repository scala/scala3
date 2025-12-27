package scala.quoted.compiletime

trait Position private[compiletime] () {

  /** The start offset in the source file. */
  def start: Int

  /** The end offset in the source file. */
  def end: Int

  /** Source file in which this position is located. */
  def sourceFile: SourceFile

  /** The start line in the source file. */
  def startLine: Int

  /** The end line in the source file. */
  def endLine: Int

  /** The start column in the source file. */
  def startColumn: Int

  /** The end column in the source file. */
  def endColumn: Int

  /** Source code within the position. */
  def sourceCode: Option[String]

}
object Position {

  def quoted(using quotes: Quotes): Position.Module = quotes.reflectV2.Position
  given moduleConversion: (quotes: Quotes) => Conversion[Position.type, Position.Module] = _ => quotes.reflectV2.Position

  trait Module private[compiletime] () {

    /** Position of the expansion site of the macro. */
    def ofMacroExpansion: Position

    /** Creates a new position in the source with the given range. The range must be contained in the file. */
    def apply(sourceFile: SourceFile, start: Int, end: Int): Position

    /** Creates a new position in the source with the given range. The range must be contained in the file. */
    def make(sourceFile: SourceFile, start: Int, end: Int): Position

  }

}
