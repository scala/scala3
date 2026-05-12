// Without the `experimental.companionScopeInference` import, a bare unresolved
// identifier is the normal "not found" error.

object NoImport:

  sealed trait Color
  object Color:
    case object Red extends Color

  val c: Color = Red // error
