// Without the `experimental.hashCompanionShorthand` import, the leading-`#`
// syntax is a parse error.

object NoImport:

  sealed trait Color
  object Color:
    case object Red extends Color

  val c: Color = #Red // error
