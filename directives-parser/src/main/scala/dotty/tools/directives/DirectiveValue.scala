package dotty.tools.directives

enum DirectiveValue:
  /** Position of this value in the source file. */
  def position: Position

  /** A string value, either quoted (`"hello"`) or bare (`hello`). */
  case StringVal(value: String, isQuoted: Boolean, position: Position)

  /** A boolean literal (`true` or `false`). */
  case BoolVal(value: Boolean, position: Position)

  /** No value was provided after the key (treated as `true` downstream). */
  case EmptyVal(position: Position)

  /** Raw source text representation (e.g. `"hello"` with quotes for quoted strings). */
  def rawText: String = this match
    case StringVal(v, true, _)  => s""""$v""""
    case StringVal(v, false, _) => v
    case BoolVal(v, _)          => v.toString
    case EmptyVal(_)            => ""

  /** The string content of the value (without quotes), or boolean/empty as string. */
  def stringValue: String = this match
    case StringVal(v, _, _) => v
    case BoolVal(v, _)      => v.toString
    case EmptyVal(_)        => ""

  /** Whether this value is a quoted string literal. */
  def isQuotedString: Boolean = this match
    case StringVal(_, true, _) => true
    case _                     => false
