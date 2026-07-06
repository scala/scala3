package dotty.tools.directives

enum Token:
  /** Absolute position of this token in the source file. */
  def pos: Position

  /** The `using` keyword. */
  case Using(pos: Position)

  /** A bare identifier or value (non-quoted, non-whitespace sequence). */
  case Ident(value: String, pos: Position)

  /** A double-quoted string literal. */
  case StringLit(value: String, pos: Position)

  /** Boolean literal `true` or `false`. */
  case BoolLit(value: Boolean, pos: Position)

  /** Dot separator in dotted keys. */
  case Dot(pos: Position)

  /** Comma – accepted as a deprecated value separator. */
  case Comma(pos: Position)

  /** End of a directive line. */
  case Newline(pos: Position)

  /** End of token stream. */
  case Eof(pos: Position)

  /** A lexer error. */
  case LexError(message: String, pos: Position)
