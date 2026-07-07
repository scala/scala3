package dotty.tools.directives

enum DirectiveValue:
  /** A string value, either quoted (`"hello"`) or bare (`hello`). */
  case StringVal(value: String, isQuoted: Boolean, position: Position)

  /** A boolean literal (`true` or `false`). */
  case BoolVal(value: Boolean, position: Position)

  /** No value was provided after the key (treated as `true` downstream). */
  case EmptyVal(position: Position)

object DirectiveValue:
  extension (dv: DirectiveValue)

    def pos: Position = dv match
      case StringVal(_, _, p) => p
      case BoolVal(_, p)      => p
      case EmptyVal(p)        => p

    /** Raw source text representation (e.g. `"hello"` with quotes for quoted strings). */
    def rawText: String = dv match
      case StringVal(v, true, _)  => s""""$v""""
      case StringVal(v, false, _) => v
      case BoolVal(v, _)          => v.toString
      case EmptyVal(_)            => ""

    /** The string content of the value (without quotes), or boolean/empty as string. */
    def stringValue: String = dv match
      case StringVal(v, _, _) => v
      case BoolVal(v, _)      => v.toString
      case EmptyVal(_)        => ""

    /** Whether this value is a quoted string literal. */
    def isQuotedString: Boolean = dv match
      case StringVal(_, true, _) => true
      case _                     => false
