import repro.Event

object Consumer:
  def consume(event: Event[String]): String = event match
    case _: Event.Item[String] => "item"
    case _                     => "status" // ok, no warning
