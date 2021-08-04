package akka.event

object TestA:
  sealed trait LogEvent

  object LogEvent:
    def myOrdinal(e: LogEvent): Int = e match
      case e: Error => 0
      // case e: Warning => 1
      case e: LogEventWithMarker => 2


  class Error() extends LogEvent
  class Error2() extends Error() with LogEventWithMarker

  // case class Warning() extends LogEvent

  sealed trait LogEventWithMarker extends LogEvent

object TestB:
  sealed trait LogEvent

  object LogEvent:
    def myOrdinal(e: LogEvent): Int = e match
      case e: Error => 0
      case e: Warning => 1
      case e: LogEventWithMarker => 2


  case class Error() extends LogEvent
  class Error2() extends Error() with LogEventWithMarker

  case class Warning() extends LogEvent

  sealed trait LogEventWithMarker extends LogEvent