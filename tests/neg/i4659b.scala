case class SourcePosition(outer: SourcePosition = NoSourcePosition) {
  assert(outer != null)  // crash
}

object NoSourcePosition extends SourcePosition() // error