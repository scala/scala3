package dotty.tools.directives

/** Public entry point for the using directives parser.
  *
  * The pipeline is:
  *   1. [[CommentExtractor.extract]] – scans the source and identifies `//> using` lines
  *   2. [[Lexer.tokenize]] – tokenizes the content of each directive line
  *   3. [[Parser.parse]] – converts the token stream into [[UsingDirective]] nodes
  */
object UsingDirectivesParser:

  /** Parse a source file and return all using directives along with diagnostics.
    *
    * @param content
    *   the raw content of the source file as a character array
    * @return
    *   a [[UsingDirectivesResult]] with parsed directives, code offset, and diagnostics
    */
  def parse(content: Array[Char]): UsingDirectivesResult =
    val extracted = CommentExtractor.extract(content)

    val allTokens = scala.collection.mutable.ArrayBuffer.empty[Token]
    for line <- extracted.directiveLines do
      val tokens = Lexer.tokenize(line.content, line.lineNum, line.lineStartOffset)
      allTokens ++= tokens

    allTokens += Token.Eof(
      extracted.directiveLines.lastOption
        .map(l => Position(l.lineNum, 0, l.lineStartOffset))
        .getOrElse(Position(0, 0, 0))
    )

    val (directives, parserDiagnostics) = Parser.parse(allTokens.toSeq)

    val allDiagnostics = parserDiagnostics ++ extracted.diagnostics

    UsingDirectivesResult(
      directives = directives,
      codeOffset = extracted.codeOffset,
      diagnostics = allDiagnostics
    )

  /** Tokenize a directive line. Exposed for testing.
    *
    * @param lineText
    *   the full line text including `//> ` prefix
    * @param lineNum
    *   0-indexed line number in the original file
    * @param lineStartOffset
    *   absolute byte offset of the start of the line
    */
  def tokenize(lineText: String, lineNum: Int, lineStartOffset: Int): Seq[Token] =
    Lexer.tokenize(lineText, lineNum, lineStartOffset)

  /** Extract directive lines from source content.  Exposed for testing. */
  def extractLines(content: Array[Char]): ExtractorResult =
    CommentExtractor.extract(content)
