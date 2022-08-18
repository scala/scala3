// scalajs: --compliant-semantics

case class SourcePosition(outer: SourcePosition = (NoSourcePosition: SourcePosition))

// The code should not compile -- currently out of reach
object NoSourcePosition extends SourcePosition()

object Test extends App {
  assert(NoSourcePosition.outer == null)
}
