package dotty.tools.directives

/** Tokenizes the content of a single `//> using` directive line.
  *
  * The lexer receives the full line text (including the `//> ` prefix), the line number in the
  * original file (0-indexed), and the absolute byte offset of the start of the line in the file. It
  * produces a sequence of [[Token]]s with positions relative to the original file.
  *
  * Comma rule: a `,` is a [[Token.Comma]] separator only if immediately followed by whitespace or
  * end of content. A `,` embedded in a non-whitespace sequence is part of the identifier/value
  * token. This matches the behaviour of the original `using_directives` Java library.
  */
object Lexer:

  /** Tokenize a directive line into a flat [[Token]] sequence.
    *
    * @param lineText
    *   the full text of the line (e.g. `//> using dep com.lihaoyi::os-lib:0.11.4`)
    * @param lineNum
    *   0-indexed line number of this line in the original file
    * @param lineStartOffset
    *   absolute byte offset of the first character of this line in the original file
    * @return
    *   tokens produced from this line, ending with [[Token.Newline]]
    */
  def tokenize(lineText: String, lineNum: Int, lineStartOffset: Int): Seq[Token] =
    val chars  = lineText.toArray
    val length = chars.length
    val buf    = scala.collection.mutable.ArrayBuffer.empty[Token]

    def pos(col: Int): Position =
      Position(lineNum, col, lineStartOffset + col)

    var col = 0

    def skipDirectivePrefix(): Unit =
      // Skip `//> ` (4 chars). The first character of useful content starts at col 4.
      col = 4

    def isWhitespace(c: Char): Boolean =
      c == ' ' || c == '\t' || c == '\r' || c == '\n' || c == '\f'

    skipDirectivePrefix()

    while col < length do
      val c = chars(col)
      if isWhitespace(c) then col += 1
      else if c == ',' && (col + 1 >= length || isWhitespace(chars(col + 1))) then
        // Standalone comma: deprecated separator
        buf += Token.Comma(pos(col))
        col += 1
      else if c == '`' then
        // Backtick-quoted identifier: strip backticks, treat content as a bare Ident
        val startCol = col
        col += 1
        val sb     = new StringBuilder
        var closed = false
        while col < length && !closed do
          chars(col) match
            case '`' =>
              closed = true
              col += 1
            case other =>
              sb += other
              col += 1
        val text = sb.toString
        if !closed then
          buf += Token.LexError("Unterminated backtick identifier", pos(startCol))
        else if text.isEmpty then
          buf += Token.LexError("Empty backtick identifier", pos(startCol))
        else
          buf += Token.Ident(text, pos(startCol))
      else if c == '"' then
        // Double-quoted string literal
        val startCol = col
        col += 1
        val sb     = new StringBuilder
        var closed = false
        while col < length && !closed do
          chars(col) match
            case '"' =>
              closed = true
              col += 1
            case '\\' if col + 1 < length =>
              col += 1
              val escaped = chars(col) match
                case 'n'                     => '\n'
                case 't'                     => '\t'
                case 'r'                     => '\r'
                case '\\'                    => '\\'
                case '"'                     => '"'
                case 'u' if col + 4 < length =>
                  val hex = lineText.substring(col + 1, col + 5)
                  col += 4
                  try Integer.parseInt(hex, 16).toChar
                  catch
                    case _: NumberFormatException =>
                      buf += Token.LexError(
                        s"Invalid unicode escape: \\u$hex",
                        pos(startCol)
                      )
                      ' '
                case other => other
              sb += escaped
              col += 1
            case other =>
              sb += other
              col += 1
        if !closed then
          buf += Token.LexError("Unterminated string literal", pos(startCol))
        else
          buf += Token.StringLit(sb.toString, pos(startCol))
      else
        // Bare identifier / value: consume until whitespace or (comma + whitespace/end)
        val startCol = col
        val sb       = new StringBuilder
        var stop     = false
        while col < length && !stop do
          val ch = chars(col)
          if isWhitespace(ch) then stop = true
          else if ch == '"' then
            buf += Token.LexError(
              "Whitespace is required between values — a quote cannot immediately follow another value.",
              pos(col)
            )
            stop = true
          else if ch == ',' && (col + 1 >= length || isWhitespace(chars(col + 1))) then
            // Comma followed by whitespace/end: stop here, don't consume the comma
            stop = true
          else
            sb += ch
            col += 1
        val text = sb.toString
        if text.nonEmpty then
          text match
            case "using" => buf += Token.Using(pos(startCol))
            case "true"  => buf += Token.BoolLit(true, pos(startCol))
            case "false" => buf += Token.BoolLit(false, pos(startCol))
            case _       =>
              // Bare tokens (including dotted keys like `test.dep`) are emitted as a single Ident.
              buf += Token.Ident(text, pos(startCol))

    buf += Token.Newline(pos(length))
    buf.toSeq
