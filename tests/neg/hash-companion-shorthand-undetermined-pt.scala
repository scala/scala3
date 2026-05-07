import scala.language.experimental.hashCompanionShorthand

object UndeterminedPt:

  sealed trait Color
  object Color:
    case object Red extends Color

  // No expected type — caller can't infer a target.
  def poly[A](a: A): A = a
  poly(#Red) // error

  // Bare expression statement; nothing constrains the expected type to Color.
  def stmt(): Unit =
    #Red // error
