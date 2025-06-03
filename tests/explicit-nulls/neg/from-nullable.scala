import scala.annotation.experimental

@experimental def testFromNullable =
  val s: String | Null = "abc"
  val sopt1: Option[String] = Option(s) // error
  val sopt2: Option[String] = Option.fromNullable(s) // ok