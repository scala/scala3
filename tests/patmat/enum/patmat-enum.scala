object Test1 {
  val day: Day = ???

  day match {
    case Day.MONDAY => true
    case Day.TUESDAY => true
    case Day.WEDNESDAY => true
  }
}

object Test2 {
  import Day.*
  val day: Day = ???

  day match {
    case MONDAY => true
    case TUESDAY => true
    case WEDNESDAY => true
    case SUNDAY => true
  }
}