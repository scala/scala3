import repro.Event_1

object Consumer:
  def consume(event: Event_1[String]): String = event match
    case _: Event_1.Item[String] => "item"
    case _                       => "status" // ok, no warning
