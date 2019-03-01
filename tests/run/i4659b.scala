case class SourcePosition(outer: SourcePosition = (NoSourcePosition: SourcePosition))

object NoSourcePosition extends SourcePosition()

object Test extends App {
  assert(NoSourcePosition.outer == null)
}