package dotty.tools.directives

/** Represents a single `//> using ...` directive line extracted from a source file. */
case class DirectiveLine(
  /** The full text of the line, including the `//> ` prefix (no trailing newline). */
  content: String,
  /** 0-indexed line number in the original source file. */
  lineNum: Int,
  /** Absolute byte offset of the first character of this line in the source file. */
  lineStartOffset: Int
)

/** Result of the comment extraction phase. */
case class ExtractorResult(
  directiveLines: Seq[DirectiveLine],
  codeOffset: Int,
  diagnostics: Seq[UsingDirectiveDiagnostic]
)

/** Phase 1: scans a source file and extracts `//> using` directive lines.
  *
  * Rules:
  *   - Lines beginning with `#!` (shebang) are allowed only as the very first line.
  *   - Blank lines are allowed anywhere in the directive region.
  *   - Line comments (`//` not `//> `) are allowed and skipped.
  *   - Block comments (`/* ... */`, including multi-line) are allowed and skipped.
  *   - The first line that is none of the above marks the start of code (`codeOffset`).
  *   - Any `//> using` lines that appear after `codeOffset` are NOT included in the result, but a
  *     warning diagnostic is emitted for each one.
  *   - Directives inside block comments are NOT parsed.
  */
object CommentExtractor:

  private val UsingDirectiveRegex = """^//>\s+using(?:\s|$)""".r

  def extract(rawContent: IndexedSeq[Char]): ExtractorResult =
    val content =
      if rawContent.nonEmpty && rawContent(0) == '\uFEFF' then rawContent.drop(1)
      else rawContent
    val length      = content.length
    val bomOffset   = rawContent.length - content.length
    val diagnostics = scala.collection.mutable.ArrayBuffer.empty[UsingDirectiveDiagnostic]
    val directives  = scala.collection.mutable.ArrayBuffer.empty[DirectiveLine]

    var offset    = 0
    var lineNum   = 0
    var codeStart = -1 // -1 means not yet found

    def currentLineText(lineStartOff: Int): String =
      var end = lineStartOff
      while end < length && content(end) != '\n' do end += 1
      content.slice(lineStartOff, end).mkString

    // Skip a block comment starting at `offset` (which points to `/` of `/*`).
    // Returns the offset just after the closing `*/`, updating `lineNum`.
    // Supports nested block comments to match the Scala compiler.
    def skipBlockComment(startOff: Int, startLine: Int): (Int, Int) =
      var off   = startOff + 2 // skip `/*`
      var ln    = startLine
      var depth = 1
      while off < length - 1 && depth > 0 do
        if content(off) == '/' && content(off + 1) == '*' then
          depth += 1
          off += 2
        else if content(off) == '*' && content(off + 1) == '/' then
          depth -= 1
          off += 2
        else
          if content(off) == '\n' then ln += 1
          off += 1
      if depth > 0 && off < length then
        if content(off) == '\n' then ln += 1
        off += 1
      (off, ln)

    while offset < length && codeStart < 0 do
      val lineStart = offset

      // Determine what kind of line this is (without consuming it yet)
      val lineContent = currentLineText(lineStart)
      val trimmed     = lineContent.stripLeading()

      if trimmed.isEmpty then
        // Blank line: skip
        offset = lineStart + lineContent.length
        if offset < length && content(offset) == '\n' then offset += 1
        lineNum += 1
      else if lineNum == 0 && trimmed.startsWith("#!") then
        // Shebang: skip only on the very first line
        offset = lineStart + lineContent.length
        if offset < length && content(offset) == '\n' then offset += 1
        lineNum += 1
      else if trimmed.startsWith("/*") then
        // Block comment: skip the whole comment, may span multiple lines
        val commentStartOff         = lineStart + lineContent.indexOf("/*")
        val (afterComment, newLine) =
          skipBlockComment(commentStartOff, lineNum)
        // After the block comment, check if the rest of the line (if any) is blank
        // Find the end of the current logical "section" that covers the block comment
        // We need to advance `offset` and `lineNum` past the block comment.
        // Also check if the block comment ends on the same line and there's more content.
        offset = afterComment
        lineNum = newLine
        // Skip to end of the current line (in case there's trailing whitespace after `*/`)
        while offset < length && content(offset) != '\n' do
          val c = content(offset)
          if c != ' ' && c != '\t' && c != '\r' then
            // Non-blank, non-comment content after `*/` on the same line → code starts
            codeStart = lineStart
          offset += 1
        if codeStart < 0 then
          if offset < length && content(offset) == '\n' then offset += 1
          lineNum += 1
      else if trimmed.startsWith("//") && !trimmed.startsWith("//>") then
        // Line comment (not a directive): skip
        offset = lineStart + lineContent.length
        if offset < length && content(offset) == '\n' then offset += 1
        lineNum += 1
      else if trimmed.startsWith("//>") then
        val withoutLeading = lineContent.dropWhile(c => c == ' ' || c == '\t')
        val leadingLen     = lineContent.length - withoutLeading.length
        if withoutLeading.startsWith("//> using") then
          val adjustedOffset = lineStart + leadingLen + bomOffset
          directives += DirectiveLine(withoutLeading, lineNum, adjustedOffset)
          offset = lineStart + lineContent.length
          if offset < length && content(offset) == '\n' then offset += 1
          lineNum += 1
        else if UsingDirectiveRegex.findFirstIn(withoutLeading).isDefined then
          val linePos = Some(Position(lineNum, leadingLen, lineStart + leadingLen + bomOffset))
          val msg     =
            s"Using directive must use the exact prefix `//> using`. Invalid prefix in: ${withoutLeading.trim}"
          diagnostics += UsingDirectiveDiagnostic(msg, DiagnosticSeverity.Warning, linePos)
          offset = lineStart + lineContent.length
          if offset < length && content(offset) == '\n' then offset += 1
          lineNum += 1
          codeStart = offset
        else
          codeStart = lineStart
      else
        // First code line
        codeStart = lineStart

    // If we never found code, codeOffset is end of file
    val codeOffset = if codeStart >= 0 then codeStart else length

    // Continue scanning the rest of the file for post-code directives
    if codeStart >= 0 then
      offset = codeStart
      var ln = lineNum

      while offset < length do
        val lineStart   = offset
        val lineContent = currentLineText(lineStart)
        val trimmed     = lineContent.stripLeading()

        if trimmed.startsWith("/*") then
          // Skip block comment
          val commentStartOff         = lineStart + lineContent.indexOf("/*")
          val (afterComment, newLine) = skipBlockComment(commentStartOff, ln)
          offset = afterComment
          ln = newLine
          while offset < length && content(offset) != '\n' do offset += 1
          if offset < length && content(offset) == '\n' then offset += 1
          ln += 1
        else
          val linePos = Some(Position(ln, 0, lineStart + bomOffset))
          if trimmed.startsWith("//> using") then
            val msg = s"Ignoring using directive found after Scala code: ${trimmed.trim}"
            diagnostics += UsingDirectiveDiagnostic(msg, DiagnosticSeverity.Warning, linePos)
          else if UsingDirectiveRegex.findFirstIn(trimmed).isDefined then
            val msg = s"Ignoring using directive found after Scala code: ${trimmed.trim}"
            diagnostics += UsingDirectiveDiagnostic(msg, DiagnosticSeverity.Warning, linePos)
          offset = lineStart + lineContent.length
          if offset < length && content(offset) == '\n' then offset += 1
          ln += 1

    ExtractorResult(directives.toSeq, codeOffset + bomOffset, diagnostics.toSeq)
