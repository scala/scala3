@main def happyBirthday(age: Int, name: String, others: String*) = {
  val suffix =
    (age % 100) match {
      case 11 | 12 | 13 => "th"
      case _ =>
        (age % 10) match {
          case 1 => "st"
          case 2 => "nd"
          case 3 => "rd"
          case _ => "th"
        }
    }
  val bldr = new StringBuilder(s"Happy $age$suffix birthday, $name")
  for other <- others do bldr.append(" and ").append(other)
  bldr.toString
}