package dotty.tools
package dotc
package cc

/** Possible kinds of captures */
enum CapturingKind:
  case Regular     // normal capture
  case Boxed       // capture under box
  case ByName      // capture applies to enclosing by-name type (only possible before ElimByName)
