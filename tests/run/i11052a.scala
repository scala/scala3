class Foo {
  implicit class StringShould(private val s: String) {
    def should(right: => Unit) = right
  }

  "Foo" should {
    object Weekdays extends Enumeration {
      val Mon, Tue, Wed, Thu, Fri, Sat, Sun = Value
    }
    println(Weekdays.Mon.toString)
  }
}

object Test {
  def main(args: Array[String]): Unit = new Foo()
}
