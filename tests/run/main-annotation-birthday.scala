/**
  * Wishes a happy birthday to lucky people!
  *
  * @param age the age of the people whose birthday it is
  * @param name the name of the luckiest person!
  * @param others all the other lucky people
  */
@main def happyBirthday(age: Int, name: String, others: String*) =
  val suffix =
    age % 100 match
    case 11 | 12 | 13 => "th"
    case _ =>
      age % 10 match
        case 1 => "st"
        case 2 => "nd"
        case 3 => "rd"
        case _ => "th"
  val bldr = new StringBuilder(s"Happy $age$suffix birthday, $name")
  for other <- others do bldr.append(" and ").append(other)
  bldr.toString

object Test:
  def callMain(args: Array[String]): Unit =
    val clazz = Class.forName("happyBirthday")
    val method = clazz.getMethod("main", classOf[Array[String]])
    method.invoke(null, args)

  def main(args: Array[String]): Unit =
    callMain(Array("23", "Lisa", "Peter"))
end Test
