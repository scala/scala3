object Test1 {
  val day: Day = ???

  day match {
    case Day.MONDAY => ()
    case Day.TUESDAY => ()
    case Day.WEDNESDAY => ()
  }
}

object Test2 {
  import Day._
  val day: Day = ???

  day match {
    case MONDAY => ()
    case TUESDAY => ()
    case WEDNESDAY => ()
    case SUNDAY => ()
  }
}