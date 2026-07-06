package dotty.tools.directives

import scala.annotation.tailrec

/** Phase 3: recursive-descent parser that consumes a token stream and produces [[UsingDirective]]
  * nodes.
  *
  * The accepted grammar is specified in the module README (the "Grammar" section of
  * `directives-parser/README.md`), which is the single source of truth for the syntax.
  *
  * When Values is empty (key immediately followed by Newline), a single [[DirectiveValue.EmptyVal]]
  * is produced.
  *
  * Error recovery: on unexpected token, a diagnostic is emitted and the parser skips to the next
  * Newline token.
  */
object Parser:

  private def tokenKindName(t: Token): String =
    t match
      case _: Token.Using     => "Using"
      case _: Token.Ident     => "Ident"
      case _: Token.StringLit => "StringLit"
      case _: Token.BoolLit   => "BoolLit"
      case _: Token.Dot       => "Dot"
      case _: Token.Comma     => "Comma"
      case _: Token.Newline   => "Newline"
      case _: Token.Eof       => "Eof"
      case _: Token.LexError  => "LexError"

  def parse(tokens: Seq[Token]): (Seq[UsingDirective], Seq[UsingDirectiveDiagnostic]) =
    val diagnostics = scala.collection.mutable.ArrayBuffer.empty[UsingDirectiveDiagnostic]
    val directives  = scala.collection.mutable.ArrayBuffer.empty[UsingDirective]
    val arr         = tokens.toIndexedSeq
    var pos         = 0

    def current: Token = if pos < arr.length then arr(pos) else Token.Eof(Position(0, 0, 0))

    def advance(): Unit = if pos < arr.length then pos += 1

    def skipToNewline(): Unit =
      @tailrec
      def skipUntilBoundary(): Unit =
        current match
          case _: Token.Newline | _: Token.Eof => ()
          case _                               =>
            advance()
            skipUntilBoundary()
      skipUntilBoundary()
      current match
        case _: Token.Newline => advance()
        case _                => ()

    def error(msg: String, p: Position): Unit =
      diagnostics += UsingDirectiveDiagnostic(msg, DiagnosticSeverity.Error, p)

    def warn(msg: String, p: Position): Unit =
      diagnostics += UsingDirectiveDiagnostic(msg, DiagnosticSeverity.Warning, p)

    def parseValues(): Seq[DirectiveValue] =
      val values = scala.collection.mutable.ArrayBuffer.empty[DirectiveValue]

      @tailrec
      def loop(): Seq[DirectiveValue] =
        current match
          case _: Token.Newline | _: Token.Eof =>
            values.toSeq
          case Token.Comma(p) =>
            if values.isEmpty then
              values += DirectiveValue.StringVal(",", isQuoted = false, p)
              advance()
              loop()
            else
              warn(
                "Use of commas as separators is deprecated. Only whitespace is necessary.",
                p
              )
              advance()
              loop()
          case Token.StringLit(v, p) =>
            values += DirectiveValue.StringVal(v, isQuoted = true, p)
            advance()
            loop()
          case Token.BoolLit(v, p) =>
            values += DirectiveValue.BoolVal(v, p)
            advance()
            loop()
          case Token.Ident(v, p) =>
            if v.endsWith(",") then
              warn(
                s"Value '$v' ends with a comma — this is likely a typo from a double-comma sequence.",
                p
              )
            values += DirectiveValue.StringVal(v, isQuoted = false, p)
            advance()
            loop()
          case Token.Using(p) =>
            values += DirectiveValue.StringVal("using", isQuoted = false, p)
            advance()
            loop()
          case Token.LexError(msg, p) =>
            error(s"Lexer error: $msg", p)
            advance()
            loop()
          case t =>
            error(s"Unexpected token in directive values: ${tokenKindName(t)}", t.pos)
            skipToNewline()
            values.toSeq

      loop()

    @tailrec
    def parseDirectives(): Unit =
      current match
        case _: Token.Eof =>
          ()
        case Token.Newline(_) =>
          advance()
          parseDirectives()
        case Token.Using(usingPos) =>
          advance()
          current match
            case Token.Ident(keyText, keyPos) =>
              advance()
              val values      = parseValues()
              val finalValues =
                if values.isEmpty then
                  Seq(DirectiveValue.EmptyVal(
                    Position(
                      keyPos.line,
                      keyPos.column + keyText.length,
                      keyPos.offset + keyText.length
                    )
                  ))
                else values
              directives += UsingDirective(keyText, finalValues, keyPos)
              current match
                case _: Token.Newline => advance()
                case _                => ()
              parseDirectives()

            case Token.Newline(_) =>
              error("Expected a key after `using`", usingPos)
              advance()
              parseDirectives()

            case _: Token.Eof =>
              error("Expected a key after `using`", usingPos)
              parseDirectives()

            case t =>
              error(s"Expected a key after `using`, found: ${tokenKindName(t)}", t.pos)
              skipToNewline()
              parseDirectives()

        case Token.LexError(msg, p) =>
          error(s"Lexer error: $msg", p)
          skipToNewline()
          parseDirectives()

        case t =>
          error(s"Unexpected token: ${tokenKindName(t)}", t.pos)
          skipToNewline()
          parseDirectives()

    parseDirectives()
    (directives.toSeq, diagnostics.toSeq)
