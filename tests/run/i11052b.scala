object Test {
  def main(args: Array[String]): Unit = {
    object Weekdays extends Enumeration {
      val Mon, Tue, Wed, Thu, Fri, Sat, Sun = Value
    }
    println(Weekdays.Mon.toString)
  }
}
